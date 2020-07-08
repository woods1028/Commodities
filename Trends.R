counties_list <- nclim_tmpc %>% 
  #filter(State %in% c("Iowa","Illinois","Nebraska","Indiana","Ohio","Kansas","South Dakota","Minnesota","Alaska","Tennessee")) %>% 
  select(State_Code,County_Code) %>% 
  distinct

tmpc_trend <- 1:nrow(counties_list) %>% 
  lapply(function(i) {
    
    print(i)
    
    df <- nclim_tmpc %>% 
      filter(State_Code==counties_list[i,"State_Code"],County_Code==counties_list[i,"County_Code"],Date>="1990-01-01")
    
    m <- df %>%
      select(ds = Date, y = Value) %>%
      mutate(cap=.5) %>% 
      prophet(
        holidays = NULL,
        daily.seasonality = F,
        weekly.seasonality = F
      )
    
    forecast <- predict(m,make_future_dataframe(m,periods = 12,freq = "month"))
    
    forecast %>% 
      mutate(
        lag=lag(trend),
        lead=lead(trend)
      ) %>% 
      mutate(
        MinMax=ifelse(lag>trend&lead>trend,"Minimum",
                      ifelse(lag<trend&lead<trend,"Maximum",NA))
      ) %>% 
      filter(is.na(lag)|is.na(lead)|!is.na(MinMax)) %>% 
      slice((nrow(.)-1):nrow(.)) %>% 
      mutate(
        trenddiff=lead(trend)-trend,
        yeardiff=as.numeric(difftime(lead(ds),ds,units = "auto")/365.25),
        per_year=trenddiff/yeardiff
      ) %>% 
      mutate(county_code=counties_list[i,"County_Code"],state_code=counties_list[i,"State_Code"]) %>% 
      slice(1) %>% 
      select(state_code,county_code,per_year)
  })

tmpc_df <- tmpc_trend %>% 
  bind_rows %>%
  left_join(
    noaa_state_codes,
    by=c("state_code"="State_Code")
  ) %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="040") %>% 
      select(State=`Area.Name.(including.legal/statistical.area.description)`,state_fips_code=`State.Code.(FIPS)`),
    by="State"
  ) %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="050") %>% 
      select(state_fips_code=`State.Code.(FIPS)`,county_code=`County.Code.(FIPS)`,County=`Area.Name.(including.legal/statistical.area.description)`),
    by=c("state_fips_code","county_code")
  )

  #   left_join(
  #   acreage %>% 
  #     filter(year %in% 2015:2019) %>% 
  #     group_by(county_code,state_fips_code) %>% 
  #     summarise_at(vars(Value),~sum(.)/n_distinct(year)) %>% 
  #     ungroup %>% 
  #     rename(Acreage=Value),
  #   by=c("county_code","state_fips_code")
  # ) %>% 
  # filter(!is.na(Acreage)) %>% 
  # list(
  #   group_by(.,State) %>% 
  #     summarise_at(vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
  #     ungroup,
  #   summarise_at(.,vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
  #     mutate(State="All")
  # ) %>% 
  # extract(-1) %>% 
  # bind_rows

pcpn_trend <- 1:nrow(counties_list) %>% 
  lapply(function(i) {
    
    print(i)
    
    df <- nclim_pcpn %>% 
      filter(State_Code==counties_list[i,"State_Code"],County_Code==counties_list[i,"County_Code"],Date>="1990-01-01")
    
    m <- df %>%
      select(ds = Date, y = Value) %>%
      mutate(cap=.5) %>% 
      prophet(
        holidays = NULL,
        daily.seasonality = F,
        weekly.seasonality = F
      )
    
    forecast <- predict(m,make_future_dataframe(m,periods = 12,freq = "month"))
    
    forecast %>% 
      mutate(
        lag=lag(trend),
        lead=lead(trend)
      ) %>% 
      mutate(
        MinMax=ifelse(lag>trend&lead>trend,"Minimum",
                      ifelse(lag<trend&lead<trend,"Maximum",NA))
      ) %>% 
      filter(is.na(lag)|is.na(lead)|!is.na(MinMax)) %>% 
      slice((nrow(.)-1):nrow(.)) %>% 
      mutate(
        trenddiff=lead(trend)-trend,
        yeardiff=as.numeric(difftime(lead(ds),ds,units = "auto")/365.25),
        per_year=trenddiff/yeardiff
      ) %>% 
      mutate(county_code=counties_list[i,"County_Code"],state_code=counties_list[i,"State_Code"]) %>% 
      slice(1) %>% 
      select(state_code,county_code,per_year)
  })

pcpn_df <- pcpn_trend %>% 
  bind_rows %>%
  left_join(
    noaa_state_codes,
    by=c("state_code"="State_Code")
  ) %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="040") %>% 
      select(State=`Area.Name.(including.legal/statistical.area.description)`,state_fips_code=`State.Code.(FIPS)`),
    by="State"
  )

  left_join(
    acreage %>% 
      filter(year %in% 2015:2019) %>% 
      group_by(county_code,state_fips_code) %>% 
      summarise_at(vars(Value),~sum(.)/n_distinct(year)) %>% 
      ungroup %>% 
      rename(Acreage=Value),
    by=c("county_code","state_fips_code")
  ) %>% 
  filter(!is.na(Acreage)) %>% 
  list(
    group_by(.,State) %>% 
      summarise_at(vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
      ungroup,
    summarise_at(.,vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
      mutate(State="All")
  ) %>% 
  extract(-1) %>% 
  bind_rows

tmax_trend <- 1:nrow(counties_list) %>% 
  lapply(function(i) {
    
    print(i)
    
    df <- nclim_tmax %>% 
      filter(State_Code==counties_list[i,"State_Code"],County_Code==counties_list[i,"County_Code"],Date>="1990-01-01")
    
    m <- df %>%
      select(ds = Date, y = Value) %>%
      mutate(cap=.5) %>% 
      prophet(
        holidays = NULL,
        daily.seasonality = F
      )
    
    forecast <- predict(m,make_future_dataframe(m,periods = 12,freq = "month"))
    
    forecast %>% 
      mutate(
        lag=lag(trend),
        lead=lead(trend)
      ) %>% 
      mutate(
        MinMax=ifelse(lag>trend&lead>trend,"Minimum",
                      ifelse(lag<trend&lead<trend,"Maximum",NA))
      ) %>% 
      filter(is.na(lag)|is.na(lead)|!is.na(MinMax)) %>% 
      slice((nrow(.)-1):nrow(.)) %>% 
      mutate(
        trenddiff=lead(trend)-trend,
        yeardiff=as.numeric(difftime(lead(ds),ds,units = "auto")/365.25),
        per_year=trenddiff/yeardiff
      ) %>% 
      mutate(county_code=counties_list[i,"County_Code"],state_code=counties_list[i,"State_Code"]) %>% 
      slice(1) %>% 
      select(state_code,county_code,per_year)
  })

tmax_trend %>% 
  bind_rows %>%
  left_join(
    noaa_state_codes,
    by=c("state_code"="State_Code")
  ) %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="040") %>% 
      select(State=`Area.Name.(including.legal/statistical.area.description)`,state_fips_code=`State.Code.(FIPS)`),
    by="State"
  ) %>% 
  left_join(
    acreage %>% 
      filter(year %in% 2015:2019) %>% 
      group_by(county_code,state_fips_code) %>% 
      summarise_at(vars(Value),~sum(.)/n_distinct(year)) %>% 
      ungroup %>% 
      rename(Acreage=Value),
    by=c("county_code","state_fips_code")
  ) %>% 
  filter(!is.na(Acreage)) %>% 
  list(
    group_by(.,State) %>% 
      summarise_at(vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
      ungroup,
    summarise_at(.,vars(per_year),~weighted.mean(.,w = Acreage)) %>% 
      mutate(State="All")
  ) %>% 
  extract(-1) %>% 
  bind_rows
