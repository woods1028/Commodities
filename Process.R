require(tidyverse)
require(readxl)
require(magrittr)
require(RSelenium)
require(rvest)
require(beepr)
require(prophet)

source("C:/Users/Sam Woods/Dropbox/Environment Calls.R")

require(tidyUSDA)
require(keyring)

#### Production ####

nass_api_key <- key_get(service = "NASS")

states_to_get <- c("IOWA","NEBRASKA","ILLINOIS") %>% 
  tibble(State=.,Type="Midwest") %>% 
  bind_rows(
    c("TENNESSEE","ALABAMA","GEORGIA","TEXAS","MISSISSIPPI","ARKANSAS") %>% 
      tibble(State=.,Type="Non-Midwest")
  ) %>% 
  mutate_at(vars(State),toupper)

acreage <- split(states_to_get$State, ceiling(seq_along(states_to_get$State)/3)) %>% 
  map(
    ~c("CORN","SOYBEANS") %>% 
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
          state = .x
        )
      })
  ) %>% 
  flatten %>% 
  map(tibble) %>% 
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
      state = states_to_get %>% 
        filter(Type=="Midwest") %>% 
        pull(State)
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
      state = states_to_get %>% 
        filter(Type=="Non-Midwest") %>% 
        pull(State)
    )
  }) %>% 
  bind_rows

beep(sound = "coin")

#### Weather ####

noaa_state_codes <- read_excel("State Codes.xlsx",sheet = 1)

fips_county_codes <- "all-geocodes-v2018.xlsx" %>% 
  read_excel(sheet = "v2018geocodes",skip = 4)

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

### This is national weather metrics by month

weather_standards <- weather_trends %>% 
  group_by(Metric,Month=format(date,"%m"),Year=format(date,"%Y")) %>% 
  summarise_at(vars(actual),list(mean=~mean(.,na.rm = T),sd = ~sd(.,na.rm = T))) %>% 
  ungroup

weather_trends <- weather_trends %>% 
  filter(toupper(State) %in% states_to_get$State)

#### Model DF ####

commodity <- "CORN"
growing_seasons <- seq(as.Date("1989-10-01"),as.Date("2021-10-01"),by = "1 year")

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

weather_v_expected <- weather_trends %>% 
  mutate(Growing_Season=cut(date,growing_seasons)) %>% 
  mutate_at(vars(Growing_Season),as.Date) %>% 
  mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer %>% add(1)) %>% 
  mutate(Month=format(date,"%m")) %>% 
  mutate_at(vars(Month),~paste(Metric,.,sep = "_")) %>% 
  mutate(Value=actual-fit) %>% 
  pivot_wider(id_cols = c(state_fips,County_Code,Growing_Season),values_from = "Value",names_from = "Month")

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

#### Yield ####

yield_trend_fn <- function(county_code,state_fips_code,commodity_desc) {
  
  df_for_fn <- yield %>% 
    filter(county_code==county_code,state_fips_code==state_fips_code,commodity_desc==commodity_desc) %>% 
    mutate(Date=paste(year,"10-01",sep = "-") %>% as.Date) 
  
  m_var <- df_for_fn %>% 
    dplyr::rename(ds = Date, y = Yield) %>% 
    prophet(
      weekly.seasonality = F,
      daily.seasonality = F,
      yearly.seasonality = F
    )
  
  m_forecast <- predict(m_var,make_future_dataframe(m_var,periods = 1,freq = "year"))
  
  m_forecast %>% 
    tibble %>% 
    left_join(
      df_for_fn %>% 
        select(ds=Date,y=Yield),
      by="ds"
    ) %>% 
    select(ds,trend,fit=yhat,actual=y) %>% 
    mutate_at(vars(ds),as.Date) %>% 
    dplyr::rename(Growing_Season=ds) %>% 
    mutate(
      county_code=county_code,
      state_fips_code=state_fips_code
    )
  
}

yield_trends <- yield %>% 
  filter(commodity_desc=="CORN") %>% 
  group_by(county_code,state_fips_code) %>% 
  filter(n()>10) %>% 
  ungroup %>% 
  mutate(id=paste(county_code,state_fips_code)) %>% 
  pull(id) %>% 
  unique %>% 
  lapply(function(i) {
    
    df_for_fn <- yield %>% 
      filter(paste(county_code,state_fips_code)==i,commodity_desc=="CORN") %>% 
      mutate(Date=paste(year,"10-01",sep = "-") %>% as.Date)
    
    m_var <- df_for_fn %>% 
      dplyr::rename(ds = Date, y = Yield) %>% 
      prophet(
        weekly.seasonality = F,
        daily.seasonality = F,
        yearly.seasonality = F,
        n.changepoints = 5
      )
    
    m_forecast <- predict(m_var,make_future_dataframe(m_var,periods = 1,freq = "year"))
    
    m_forecast %>% 
      tibble %>% 
      left_join(
        df_for_fn %>% 
          select(ds=Date,y=Yield),
        by="ds"
      ) %>% 
      select(ds,trend,fit=yhat,actual=y) %>% 
      dplyr::rename(Growing_Season=ds) %>% 
      mutate(id=i)
    
  }) %>% 
  bind_rows %>% 
  mutate_at(vars(Growing_Season),as.Date)

dirt <- "Dirt By County.csv" %>% 
  read_csv

yield_to_model <- yield %>% 
  filter(commodity_desc=="CORN") %>% 
  left_join(
    weather_v_center_by_gs,
    by=c("state_fips_code"="state_fips","county_code"="County_Code","year"="Growing_Season")
  ) %>% 
  left_join(
    select(.,year,county_code,state_fips_code,commodity_desc,Yield_LY=Yield) %>% 
      mutate_at(vars(year),~.+1),
    by=c("year","county_code","state_fips_code","commodity_desc")
  ) %>% 
  left_join(
    dirt,
    by=c("county_code","state_fips_code"="state_code")
  ) %>% 
  tibble

yield_to_model2 <- yield_trends %>% 
  mutate_at(vars(Growing_Season),~format(.,"%Y") %>% as.integer) %>% 
  separate(id,c("County_Code","State_Code"),sep = " ") %>% 
  left_join(
    weather_v_expected,
    by=c("State_Code"="state_fips","County_Code","Growing_Season")
  ) %>% 
  left_join(
    dirt %>% 
      select(-c(matches("70,100"))),
    by=c("State_Code"="state_code","County_Code"="county_code")
  ) %>% 
.[complete.cases(.),]


