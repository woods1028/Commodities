require(tidyverse)
require(tidymodels)
require(cowplot)
require(vip)
require(stacks)
require(poissonreg)
doParallel::registerDoParallel()

#### Model Maker ####

model_maker <- function(model_name,model_mode,model_engine,model_pkg,model_defaults) {
  
  set_new_model(model_name)
  set_model_mode(model = model_name,mode = model_mode)
  set_model_engine(
    model_name,
    mode = model_mode,
    eng = model_engine
  )
  
  set_fit(
    model = model_name,
    mode = model_mode,
    eng = model_engine,
    value = list(
      interface = "formula",
      protect = c("formula","data"),
      func = c(pkg = model_pkg, fun = model_engine),
      defaults = model_defaults
    )
  )
  
  set_encoding(
    model = model_name,
    eng = model_engine,
    mode = model_mode,
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = T,
      remove_intercept = T,
      allow_sparse_x = F
    )
  )
  set_pred(
    model = model_name,
    eng = model_engine,
    mode = model_mode,
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
    )
  )
  
  parsnip::set_dependency(model_name,model_engine,model_pkg)
  
  model_type <- function(mode = model_mode) {
    
    new_model_spec(
      model_name,
      args = NULL,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
    
  }
  
  return(model_type)
  
}

gamma_reg <- model_maker(
  model_name = "gamma_reg",
  model_mode = "regression",
  model_engine = "glm",
  model_pkg = "stats",
  model_defaults = list(family = expr(Gamma(link = "log")))
)

#### Stacker Function ####

stacker_fn <- function(dataframe, recipe_to_use) {
  
  ctrl_grid <- control_stack_grid()
  ctrl_res <- control_stack_resamples()

  folds <- vfold_cv(dataframe,v = 5)
  
  metric <- metric_set(rmse)
  
  ols_spec <- linear_reg() %>% 
    set_engine("lm")
  
  knn_spec <- nearest_neighbor(mode = "regression",neighbors = tune("k")) %>% 
    set_engine("kknn")
  
  forest_spec <- rand_forest(
    mode = "regression",
    mtry = tune(),
    min_n = tune()
  ) %>% 
    set_engine("ranger",importance = "impurity")

  gamma_spec <- gamma_reg() %>% 
    set_engine("glm")
  
  # return(
  #   ls(pattern = "spec",name = sys.frame(which = 1)) %>% 
  #     mget(inherits = T)
  # )
    
  wflows <- ls(pattern = "spec",name = sys.frame(which = 1)) %>% 
    mget(inherits = T) %>% 
    tibble(Type=names(.),Spec = .) %>% 
    mutate(wflow=map(
      Spec,
      ~workflow() %>% 
        add_model(.x) %>% 
        add_recipe(recipe_to_use))
    ) %>% 
    mutate_at(vars(Type),~sub("_spec","",.))
  
  ols_res <- fit_resamples(
    wflows[wflows$Type=="ols","wflow"][[1]][[1]],
    resamples = folds,
    metrics = metric,
    control = ctrl_res
  )
  
  knn_res <- tune_grid(
    wflows[wflows$Type=="knn","wflow"][[1]][[1]],
    resamples = folds,
    metrics = metric,
    grid = 4,
    control = ctrl_grid
  )
  
  mtry_range <- recipe_to_use$var_info %>% 
    filter(role=="predictor") %>% 
    nrow %>% 
    list(
      .^.5,
      .^.75
    ) %>% 
    .[-1] %>% 
    unlist %>% 
    floor
  
  start_time <- Sys.time()
  forest_res <- tune_grid(
    wflows[wflows$Type=="forest","wflow"][[1]][[1]],
    resamples = folds,
    metrics = metric,
    grid = grid_regular(
      mtry(range = mtry_range),
      min_n(range(c(2,8))),
      levels = 5
    ),
    control = ctrl_grid
  ) 
  end_time <- Sys.time()
  print(end_time - start_time)
  
  gamma_res <- fit_resamples(
    wflows[wflows$Type=="gamma","wflow"][[1]][[1]],
    resamples = folds,
    metrics = metric,
    control = ctrl_res
  )
  
  member_stack <- stacks() %>% 
    add_candidates(ols_res) %>% 
    add_candidates(knn_res) %>% 
    add_candidates(forest_res) %>% 
    add_candidates(gamma_res)
  
  member_stack <- member_stack %>% 
    blend_predictions()
  
  member_stack <- member_stack %>% 
    fit_members()
  
  return(member_stack)

}

model_split <- yield_to_model %>% 
  .[complete.cases(.),] %>% 
  select(year,Yield,matches("pcpn"),matches("tmax"),matches("tmin"),matches("tmpc"),matches("LY"),matches("CEC"),matches("PH")) %>% 
  initial_split(prop = .8)

model_train <- training(model_split)
model_test <- testing(model_split)

model_recipe <- recipe(Yield~.,data = model_train)

member_stack <- stacker_fn(dataframe = model_train,recipe_to_use = model_recipe)

stack_preds <- model_test %>% 
  mutate(Type="Test") %>% 
  bind_rows(
    model_train %>% 
      mutate(Type="Train")
  ) %>% 
  bind_cols(
    predict(member_stack,.,members = T)
  ) %>% 
  mutate_at(vars(Type),~factor(.,levels = c("Train","Test"))) %>% 
  select(Growing_Season,Yield,.pred,matches("res"),Type)

stack_preds %>% 
  pivot_longer(cols = matches("res"),names_to = "Member",values_to = "Value") %>% 
  ggplot(mapping = aes(x=.pred,y=Yield,color = Growing_Season))+
  geom_point(mapping = aes(x=.pred,y=Value),color = "gray50",alpha = .5,size = 2)+
  geom_point(size = 2)+
  geom_abline(slope = 1,intercept = 0,lwd = 1)+
  facet_wrap(vars(Type))

stack_preds %>% 
  pivot_longer(cols = matches("res"),names_to = "Member",values_to = "Value") %>%
  mutate(residual_member=Value-Yield,
         residual_pred=.pred-Yield) %>% 
  ggplot(mapping = aes(x=.pred,y=residual_pred,color = Growing_Season))+
  geom_point(mapping = aes(x=.pred,y=residual_member),color = "gray50",alpha = .5,size = 2)+
  geom_point(size = 2)+
  geom_smooth(method = "loess",formula = "y~x",se = F,color = "black",lwd = 1.5)+
  facet_wrap(vars(Type))

