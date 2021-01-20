## This includes one year of future trends 

weather_trends <- "Weather Trends.csv" %>% 
  read_csv %>% 
  mutate(
    State_Code=substr(id,1,regexpr(" ",id)-1) %>% as.integer,
    County_Code=substr(id,regexpr(" ",id)+1,nchar(id))
  ) %>% 
  left_join(
    noaa_state_codes,
    by="State_Code"
  ) %>% 
  left_join(
    fips_county_codes %>% 
      filter(`Summary Level`=="040") %>% 
      select(state_fips=`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
    by="State"
  ) 

### This may need to be done by year also 

weather_standards <- weather_trends %>% 
  group_by(Metric,Month=format(date,"%m"),Year=format(date,"%Y")) %>% 
  summarise_at(vars(actual),list(mean=~mean(.,na.rm = T),sd = ~sd(.,na.rm = T))) %>% 
  ungroup


weather_trends <- weather_trends %>% 
  filter(toupper(State) %in% states_to_get)



#### actual less expected ####

weather_v_expected <- weather_trends %>% 
  mutate(Growing_Season=cut(date,growing_seasons)) %>% 
  mutate_at(vars(Growing_Season),as.Date) %>% 
  mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer %>% add(1)) %>% 
  mutate(Month=format(date,"%m")) %>% 
  mutate_at(vars(Month),~paste(Metric,.,sep = "_")) %>% 
  mutate(Value=actual-fit) %>% 
  pivot_wider(id_cols = c(state_fips,County_Code,Growing_Season),values_from = "Value",names_from = "Month")

weather_v_center <- weather_trends %>% 
  #tail(100000) %>% 
  # mutate(Growing_Season=cut(date,growing_seasons)) %>% 
  # mutate_at(vars(Growing_Season),as.Date) %>% 
  # mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer %>% add(1)) %>% 
  mutate(Month=format(date,"%m"),
         Year=format(date,"%Y")) %>% 
  left_join(
    weather_standards %>% 
      select(Metric,Month,center=mean,Year),
    by=c("Metric","Month","Year")
  ) %>% 
  mutate(Feature=paste(Metric,Month,sep = "_")) %>% 
  mutate(Value=actual-center) %>% 
  pivot_wider(id_cols = c(state_fips,County_Code,Year),values_from = "Value",names_from = "Feature")

#### Weather PCA ####

pca_recipe <- weather_v_center %>% 
  .[complete.cases(.),] %>% 
  recipe(~.,data = .) %>% 
  update_role(state_fips,County_Code,Year,new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_recipe)

tidied_pca <- tidy(pca_prep,2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

juice(pca_prep) %>%
  left_join(
    fips_county_codes %>% 
      filter(`Summary Level`=="040") %>% 
      select(state_fips=`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
    by="state_fips"
  ) %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = State), alpha = 0.7, size = 2) +
  labs(color = NULL)

#### Dirt PCA ####

pca_recipe <- dirt %>% 
  left_join(
    fips_county_codes %>% 
      filter(`Summary Level`=="040") %>% 
      select(state_fips=`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
    by=c("state_code"="state_fips")
  ) %>% 
  filter(toupper(State) %in% states_to_get) %>% 
  .[complete.cases(.),] %>% 
  recipe(~.,data = .) %>% 
  update_role(state_code,county_code,State,new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_recipe)

tidied_pca <- tidy(pca_prep,2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = State), alpha = 0.7, size = 3) +
  labs(color = NULL)

prcomp_pca <- weather_v_center %>% 
  .[complete.cases(.),] %>% 
  select(-c(state_fips,County_Code,Year)) %>% 
  prcomp

biplot(prcomp_pca)

summary(prcomp_pca)

#### Dirt + Weather PCA ####

pca_recipe <- weather_v_center %>% 
  left_join(
    dirt,
    by=c("state_fips"="state_code","County_Code"="county_code")
  ) %>% 
  .[complete.cases(.),] %>% 
  recipe(~.,data = .) %>% 
  update_role(state_fips,County_Code,Year,new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_recipe)

tidied_pca <- tidy(pca_prep,2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

juice(pca_prep) %>%
  left_join(
    fips_county_codes %>% 
      filter(`Summary Level`=="040") %>% 
      select(state_fips=`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
    by="state_fips"
  ) %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = State), alpha = 0.7, size = 2) +
  labs(color = NULL)

prcomp_pca <- weather_v_center %>% 
  left_join(
    dirt,
    by=c("state_fips"="state_code","County_Code"="county_code")
  ) %>% 
  .[complete.cases(.),] %>% 
  select(-c(state_fips,County_Code,Year)) %>% 
  prcomp

biplot(prcomp_pca)

summary(prcomp_pca)

#### Dirt + Weather + Yield PCA ####

weather_v_center_by_gs <- weather_trends %>% 
  mutate(Growing_Season=cut(date,growing_seasons)) %>%
  mutate_at(vars(Growing_Season),as.Date) %>%
  mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer %>% add(1)) %>%
  mutate(Month=format(date,"%m"),
         Year=format(date,"%Y")) %>% 
  left_join(
    weather_standards %>% 
      select(Metric,Month,center=mean,Year),
    by=c("Metric","Month","Year")
  ) %>% 
  mutate(Feature=paste(Metric,Month,sep = "_")) %>% 
  mutate(Value=actual-center) %>% 
  pivot_wider(id_cols = c(state_fips,County_Code,Growing_Season),values_from = "Value",names_from = "Feature")

pca_recipe <- weather_v_center_by_gs %>% 
  left_join(
    dirt,
    by=c("state_fips"="state_code","County_Code"="county_code")
  ) %>% 
  left_join(
    yield %>% 
      filter(commodity_desc=="CORN") %>% 
      select(state_fips_code,county_code,year,Yield),
    by=c("state_fips"="state_fips_code","County_Code"="county_code","Growing_Season"="year")
  ) %>% 
  .[complete.cases(.),] %>% 
  recipe(~.,data = .) %>% 
  update_role(state_fips,County_Code,Growing_Season,new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_recipe)

tidied_pca <- tidy(pca_prep,2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

juice(pca_prep) %>%
  left_join(
    fips_county_codes %>% 
      filter(`Summary Level`=="040") %>% 
      select(state_fips=`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
    by="state_fips"
  ) %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = State), alpha = 0.7, size = 2) +
  labs(color = NULL)

prcomp_pca <- weather_v_center_by_gs %>% 
  left_join(
    dirt,
    by=c("state_fips"="state_code","County_Code"="county_code")
  ) %>% 
  left_join(
    yield %>% 
      filter(commodity_desc=="CORN") %>% 
      select(state_fips_code,county_code,year,Yield),
    by=c("state_fips"="state_fips_code","County_Code"="county_code","Growing_Season"="year")
  ) %>% 
  .[complete.cases(.),] %>% 
  select(-c(state_fips,County_Code,Growing_Season)) %>% 
  prcomp

biplot(prcomp_pca)

summary(prcomp_pca)
