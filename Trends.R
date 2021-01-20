require(prophet)

weather_trends <- c("pcpncy","tmincy","tmaxcy","tmpccy") %>% 
  lapply(function(i) {
    
    nClimDiv[[i]] %>% 
      mutate(id=paste(State_Code,County_Code)) %>% 
      pull(id) %>% 
      unique %>% 
      lapply(function(j) {
        
        df <- nClimDiv[[i]] %>% 
          filter(paste(State_Code,County_Code)==j) %>% 
          rename(ds = Date,y = Value)
        
        county <- df %>% 
          mutate(County=paste(County_Name,State,sep = ", ")) %>% 
          pull(County) %>% 
          unique
        
        print(county)
        
        m_var <- df %>% 
          prophet(
            weekly.seasonality = F,
            daily.seasonality = F
          )
        
        m_forecast <- predict(m_var,make_future_dataframe(m_var,periods = 12,freq = "month"))
        
        m_forecast %>% 
          left_join(
            df %>% 
              select(ds,y),
            by="ds"
          ) %>% 
          select(ds,trend,fit=yhat,actual=y) %>% 
          tibble %>% 
          mutate(id=j)
        
      }) %>% 
      bind_rows %>% 
      mutate(Metric=i)
    
  })
   
weather_trends %>% 
  bind_rows %>% 
  mutate_at(vars(ds),as.Date) %>% 
  rename(date=ds) %>% 
  write_csv("Weather Trends.csv")
