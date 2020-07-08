
nass_api_key <- "API Keys.xlsx" %>% 
  read.xlsx(sheet = 1) %>% 
  filter(Type=="NASS") %>% 
  pull(Key)

acreage <- c("CORN","SOYBEANS") %>% 
  lapply(function(x) {
    getQuickstat(
      key = nass_api_key,
      program = "SURVEY",
      sector = "CROPS",
      group = "FIELD CROPS",
      commodity = x,
      category = "AREA PLANTED",
      data_item = paste(x,"ACRES PLANTED",sep = " - "),
      geographic_level = "COUNTY",
      state = c("IOWA","NEBRASKA","ILLINOIS","INDIANA","OHIO","KANSAS")
    )
  }) %>% 
  bind_rows

production1 <- c("CORN","SOYBEANS") %>% 
  lapply(function(x) {
    
    if (x=="CORN") {
      data_item1 <- "CORN, GRAIN - PRODUCTION, MEASURED IN BU"
    } else if(x=="SOYBEANS") {
      data_item1 <- "SOYBEANS - PRODUCTION, MEASURED IN BU"
    }
    
    getQuickstat(
      key = nass_api_key,
      program = "SURVEY",
      sector = "CROPS",
      group = "FIELD CROPS",
      commodity = x,
      category = "PRODUCTION",
      data_item = data_item1,
      geographic_level = "COUNTY",
      state = c("IOWA","NEBRASKA","ILLINOIS","INDIANA")
    )
  }) %>% 
  bind_rows

production2 <- c("CORN","SOYBEANS") %>% 
  lapply(function(x) {
    
    if (x=="CORN") {
      data_item1 <- "CORN, GRAIN - PRODUCTION, MEASURED IN BU"
    } else if(x=="SOYBEANS") {
      data_item1 <- "SOYBEANS - PRODUCTION, MEASURED IN BU"
    }
    
    getQuickstat(
      key = nass_api_key,
      program = "SURVEY",
      sector = "CROPS",
      group = "FIELD CROPS",
      commodity = x,
      category = "PRODUCTION",
      data_item = data_item1,
      geographic_level = "COUNTY",
      state = c("OHIO","KANSAS","MINNESOTA","SOUTH DAKOTA")
    )
  }) %>% 
  bind_rows

# acreage %>% 
#   filter(year>1990) %>% 
#   filter(reference_period_desc=="YEAR",county_name!="OTHER (COMBINED) COUNTIES") %>% 
#   left_join(
#     bind_rows(
#       production1,
#       production2
#     ) %>% 
#       filter(
#         regexpr("IRRIGATED",short_desc)<0,
#         regexpr("FOLLOWING",short_desc)<0,
#         regexpr("IN BU",short_desc)>0
#       ) %>% 
#       filter(reference_period_desc=="YEAR") %>% 
#       select(reference_period_desc,year,commodity_desc,Bushels=Value,state_alpha,county_name),
#     by=c("commodity_desc","year","reference_period_desc","state_alpha","county_name")
#   ) %>% 
#   mutate(Yield=Bushels/Value) %>% 
#   filter(year>=2015,commodity_desc=="CORN") %>% 
#   arrange(-Yield) %>%
#   #group_by(state_name) %>% slice(1) %>% ungroup %>% 
#   group_by(county_name,state_name) %>%
#   summarise_at(vars(Yield),mean) %>%
#   ungroup %>%
#   arrange(-Yield) %>% 
#   filter(state_name=="IOWA")
#   #select(county_name,state_name,year,Yield) %>% head(25) %>% arrange(-Yield)
#   select(county_name,state_name,Yield) %>% head(25) %>% arrange(-Yield) %>% data.frame

#### Modeling ####

growing_seasons <- seq(as.Date("1989-10-01"),as.Date("2020-10-01"),by = "1 year")

yield <- acreage %>% 
  filter(year>1990) %>% 
  filter(reference_period_desc=="YEAR",county_name!="OTHER (COMBINED) COUNTIES") %>% 
  left_join(
    bind_rows(
      production1,
      production2
    ) %>% 
      filter(
        regexpr("IRRIGATED",short_desc)<0,
        regexpr("FOLLOWING",short_desc)<0,
        regexpr("IN BU",short_desc)>0
      ) %>% 
      filter(reference_period_desc=="YEAR") %>% 
      select(reference_period_desc,year,commodity_desc,Bushels=Value,state_alpha,county_name),
    by=c("commodity_desc","year","reference_period_desc","state_alpha","county_name")
  ) %>% 
  mutate(Yield=Bushels/Value) %>% 
  select(year,county_name,state_name,county_code,state_fips_code,commodity_desc,Acres=Value,Bushels,Yield)
  
weather <- c("nclim_pcpn","nclim_tmin","nclim_tmax","nclim_tmpc") %>% 
  lapply(function(x) {
    x %>% 
      get %>% 
      mutate(Growing_Season=cut(Date,growing_seasons)) %>% 
      mutate_at(vars(Growing_Season),as.Date) %>% 
      mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer %>% add(1)) %>% 
      mutate(Month=format(Date,"%m")) %>% 
      mutate_at(vars(Month),~paste(.,str_extract(x,"_[a-z]+$"),sep = "")) %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary.Level=="040") %>% 
          select(state_fips=`State.Code.(FIPS)`,State=`Area.Name.(including.legal/statistical.area.description)`),
        by="State"
      ) %>% 
      setDT %>% 
      dcast(state_fips+County_Code+Growing_Season~Month,value.var="Value")
  }) %>% 
  join_all(
    by = c("state_fips","County_Code","Growing_Season")
  )

yield_to_model <- yield %>% 
  left_join(
    weather %>% 
      mutate_at(vars(matches("\\d")),scale),
    by=c("state_fips_code"="state_fips","county_code"="County_Code","year"="Growing_Season")
  ) %>% 
  filter(commodity_desc=="CORN",state_name %in% c("IOWA","ILLINOIS","NEBRASKA")) %>% 
  left_join(
    select(.,year,county_code,state_fips_code,commodity_desc,Yield_LY=Yield) %>% 
      mutate_at(vars(year),~.+1),
    by=c("year","county_code","state_fips_code","commodity_desc")
  ) %>% 
  left_join(
    dirt %>% 
      mutate_at(vars(matches("hzdep")),~./2.54) %>% 
      mutate(Depth=(hzdept_r+hzdepb_r)/2) %>% 
      mutate(Depth_Bucket=cut(Depth,depth_buckets)) %>% 
      group_by(state_name,county_code,state_code,mukey,Depth_Bucket) %>% 
      summarise_at(vars(cec7_r,hzdepb_r,ph1to1h2o_r),~weighted.mean(.,w = comppct_r,na.rm = T)) %>% 
      group_by(state_name,county_code,state_code,Depth_Bucket) %>% 
      summarise_at(vars(cec7_r,hzdepb_r,ph1to1h2o_r),~mean(.,na.rm = T)) %>% 
      ungroup %>% 
      rename(CEC=cec7_r,PH=ph1to1h2o_r) %>% 
      filter(!is.na(Depth_Bucket)) %>% 
      setDT %>% 
      dcast(state_code+county_code~Depth_Bucket,value.var=c("CEC","PH")) %>% 
      mutate_at(vars(matches("^PH")),~.-6.3),
    by=c("county_code","state_fips_code"="state_code")
  )

require(glmnet)
require(glmnetUtils)

outputs <- 1:100 %>% 
  lapply(function(x) {
    
    train <- yield_to_model %>% 
      nrow %>% 
      sample(.7*.)
    
    lasso_fit <- glmnet(
      formula = Yield~.,
      data = yield_to_model %>% 
        select(Yield,matches("pcpn"),matches("tmpc"),matches("tmax"),matches("tmin"),matches("LY"),matches("CEC"),matches("PH")),
      subset = train
    )
    
    lasso_fit_std <- glmnet(
      formula = Yield~.,
      data = yield_to_model %>% 
        mutate_at(vars(matches("pcpn"),matches("tmpc"),matches("tmax"),matches("tmin"),matches("LY"),matches("CEC"),matches("PH")),scale) %>% 
        select(Yield,matches("pcpn"),matches("tmpc"),matches("tmax"),matches("tmin"),matches("LY"),matches("CEC"),matches("PH")),
      subset = train
    )
    
    coeffs <- coefficients(lasso_fit_std,s = min(lasso_fit_std$lambda)) %>% 
      as.matrix %>% 
      data.frame(
        Names=row.names(.),
        Coefficient=.,
        stringsAsFactors = F,
        row.names = NULL
      )
    
    errors <- yield_to_model %>% 
      slice(-train) %>% 
      extract(complete.cases(.),) %>% 
      mutate(pred=predict(lasso_fit,.,s = min(lasso_fit$lambda)) %>% extract(,1)) %>% 
      mutate(Diff=pred-Yield) %>% 
      pull(Diff) %>% 
      quantile(na.rm = T) %>% 
      data.frame(
        Quantile=names(.),
        Error=.,
        stringsAsFactors = F,
        row.names = NULL
      )
    
    list(coeffs,errors)
    
  })

require(purrr,exclude = "transpose")

outputs %>% 
  flatten %>% 
  extract(seq(1,199,by = 2)) %>% 
  bind_rows %>% 
  group_by(Names) %>% 
  summarise_at(vars(X1),mean) %>% 
  ungroup %>% 
  View

outputs %>% 
  flatten %>% 
  extract(seq(2,200,by = 2)) %>% 
  bind_rows %>% 
  mutate_at(vars(Quantile),~factor(.,levels = c("0%","25%","50%","75%","100%"))) %>% 
  group_by(Quantile) %>% 
  summarise_at(vars(Error),mean) %>% 
  ungroup

lasso_fit <- glmnet(
  formula = Yield~.,
  data = yield_to_model %>% 
    select(Yield,matches("pcpn"),matches("tmpc"),matches("tmax"),matches("tmin"),matches("LY"),matches("CEC"),matches("PH"))
)

yield_to_model %>% 
  #slice(-train) %>% 
  extract(complete.cases(.),) %>% 
  mutate(pred=predict(lasso_fit,.,s = min(lasso_fit$lambda)) %>% extract(,1)) %>% 
  mutate(diff=pred-Yield) %>% 
  mutate(textlab=ifelse(abs(diff)>75,paste(county_name,year,sep = ", "),NA)) %>% 
  ggplot(mapping = aes(x=pred,y=Yield,color = year,shape = state_name))+
  geom_point(size = 2.5)+
  geom_label_repel(mapping = aes(label=textlab),color = "black")+
  scale_x_continuous(limits = c(25,250))+
  scale_y_continuous(limits = c(25,250))+
  geom_smooth(method = "lm",formula = "y~x")+
  labs(y="Yield",x="xYield",color="",shape = "")+
  guides(shape = guide_legend(override.aes = list(size = 3.5)))

outliers <- yield_to_model %>% 
  extract(complete.cases(.),) %>% 
  mutate(pred=predict(lasso_fit,.,s = min(lasso_fit$lambda)) %>% extract(,1)) %>% 
  mutate(diff=Yield-pred) %>% 
  filter(abs(diff)>50) %>% 
  group_by(county_name,Direction=sign(diff)) %>% 
  summarise_at(vars(Instances=year),n_distinct) %>% 
  ungroup %>% 
  arrange(-Instances) %>% 
  mutate_at(vars(Direction),~ifelse(Direction==-1,"Down","Up"))
