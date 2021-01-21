lasso_fit <- glmnet(
  formula = Yield~.,
  data = yield_to_model %>% 
    select(Yield,matches("pcpn"),matches("tmpc"),matches("tmax"),matches("tmin"),matches("LY"),matches("CEC"),matches("PH")),# %>% 
  #mutate_at(vars(Yield),log),
  subset = train,
  family = gaussian()
)

yield_to_model %>% 
  slice(test) %>% 
  .[complete.cases(.),] %>% 
  mutate(Fit=predict(lasso_fit,.,s = min(lasso_fit$lambda)),
         #Fit=exp(Fit),
         Residual = Fit - Yield) %>% 
  ggplot(mapping = aes(x = Fit,y=Residual))+
  geom_point()+
  geom_smooth(method = "loess",formula = "y~x")

yield_to_model %>% 
  slice(test) %>% 
  .[complete.cases(.),] %>% 
  mutate(Fit=predict(lasso_fit,.,s = min(lasso_fit$lambda)),
         Residual = Fit - Yield) %>% 
  pull(Residual) %>% 
  hist

require(tidymodels)
require(cowplot)
require(stacks)

model_split <- yield_to_model %>% 
  .[complete.cases(.),] %>% 
  select(year,Yield,matches("pcpn"),matches("tmax"),matches("tmin"),matches("tmpc"),matches("LY"),matches("CEC"),matches("PH")) %>% 
  initial_split(prop = .7)

model_train <- training(model_split)
model_test <- testing(model_split)
model_cv <- vfold_cv(model_train)

lasso_lm <- linear_reg(penalty = tune(),mixture = 1) %>% 
  set_engine("glmnet")

yield_recipe <- recipe(model_train,formula = Yield~.)

yield_workflow <- workflow() %>% 
  add_recipe(yield_recipe) %>% 
  add_model(lasso_lm)

yield_fit <- fit(
  yield_workflow,
  data = model_train
)

res <- yield_workflow %>%
  tune_grid(
    resamples = model_cv,
    grid = 10,
    metrics = yardstick::metric_set(yardstick::rmse)
  )

best_params <- res %>%
  select_best(metric = "rmse")

reg_res <- yield_workflow %>%
  finalize_workflow(best_params) %>%
  fit(data = model_train)

preds <- reg_res %>% 
  predict(new_data = model_test) %>% 
  bind_cols(
    model_test
  ) %>% 
  mutate(Type="Test") %>% 
  bind_rows(
    reg_res %>% 
      predict(new_data = model_train) %>% 
      bind_cols(
        model_train
      ) %>% 
      mutate(Type="Train")
  ) %>% 
mutate(residual=.pred-Yield)

plot_grid(
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "loess",formula = "y~x"),
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "lm",formula = "y~x"),
  nrow = 2
)

preds %>% 
  filter(Type=="Test") %>% 
  pull(residual) %>% 
  abs %>% 
  summary

reg_res %>% 
  pull_workflow_fit %>% 
  vip

#### Lognormal ####
  
yield_workflow <- update_recipe(
  yield_workflow,
  recipe(
    formula = Yield~.,
    data = model_train
  ) %>% 
    step_log(Yield,skip = T)
)
  
yield_fit <- fit(
  yield_workflow,
  data = model_train
)

res <- yield_workflow %>%
  tune_grid(
    resamples = model_cv,
    grid = 10,
    metrics = yardstick::metric_set(yardstick::rmse)
  )

best_params <- res %>%
  select_best(metric = "rmse")

reg_res <- yield_workflow %>%
  finalize_workflow(best_params) %>%
  fit(data = model_train)

preds <- reg_res %>% 
  predict(new_data = model_test %>% 
            mutate_at(vars(Yield),log)) %>% 
  bind_cols(
    model_test %>% 
      mutate_at(vars(Yield),log)
  ) %>% 
  mutate(Type="Test") %>% 
  bind_rows(
    reg_res %>% 
      predict(new_data = model_train %>% 
                mutate_at(vars(Yield),log)) %>% 
      bind_cols(
        model_train %>% 
          mutate_at(vars(Yield),log)
      ) %>% 
      mutate(Type="Train")
  ) %>% 
  mutate_at(vars(.pred,Yield),exp) %>% 
  mutate(residual=.pred-Yield)

plot_grid(
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "loess",formula = "y~x"),
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "lm",formula = "y~x"),
  nrow = 2
)

preds %>% 
  filter(Type=="Test") %>% 
  pull(residual) %>% 
  abs %>% 
  summary

reg_res %>% 
  pull_workflow_fit %>% 
  vip

#### Forest ####

forest_model <- rand_forest(mtry = 10,mode = "regression") %>% 
  set_engine("randomForest")

yield_workflow <- update_recipe(
  yield_workflow,
  recipe(
    formula = Yield~.,
    data = model_train
  )
) %>% 
  update_model(
    spec = forest_model
  )

yield_fit <- fit(
  yield_workflow,
  data = model_train
)

# res <- yield_workflow %>%
#   tune_grid(
#     resamples = model_cv,
#     grid = 10,
#     metrics = yardstick::metric_set(yardstick::rmse)
#   )
# 
# best_params <- res %>%
#   select_best(metric = "rmse")

reg_res <- yield_workflow %>%
  #finalize_workflow(best_params) %>%
  fit(data = model_train)

preds <- reg_res %>% 
  predict(new_data = model_test) %>% 
  bind_cols(
    model_test
  ) %>% 
  mutate(Type="Test") %>% 
  bind_rows(
    reg_res %>% 
      predict(new_data = model_train) %>% 
      bind_cols(
        model_train
      ) %>% 
      mutate(Type="Train")
  ) %>% 
  mutate(residual=.pred-Yield)

plot_grid(
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "loess",formula = "y~x"),
  preds %>% 
    ggplot(mapping = aes(x=.pred,y=residual))+
    geom_point()+
    facet_wrap(vars(Type))+
    geom_smooth(method = "lm",formula = "y~x"),
  nrow = 2
)

preds %>% 
  filter(Type=="Test") %>% 
  pull(residual) %>% 
  abs %>% 
  summary

reg_res %>% 
  pull_workflow_fit %>% 
  vip
