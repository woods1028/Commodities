

noaa_api_key <- "API Keys.xlsx" %>% 
  read.xlsx(sheet = 1) %>% 
  filter(Type=="NOAA") %>% 
  pull(Key)

days_to_get <- seq(Sys.Date()-days(30),Sys.Date()-1,by = "1 day")

prep <- days_to_get %>% 
  lapply(function(x) {
    print(x)
    cpc_prcp(
      date = x,
      drop_undefined = T
    ) %>% 
      mutate(date=x)
  })

prep %>% 
  bind_rows %>% 
  group_by(lon,lat) %>% 
  summarise_at(vars(precip),sum) %>% 
  ungroup %>% 
  filter(
    between(lon,230,300),
    between(lat,25,50)
  ) %>% 
  mutate_at(vars(precip),~./(2.54*100)) %>% 
  ggplot(aes(x=lon,y=lat,color=precip))+
  geom_point()

days_to_get <- seq(Sys.Date()-days(30),Sys.Date()-days(1),by = "1 day") %>% 
  as.character

locs <- ncdc_locs(
  token = noaa_api_key,
  datasetid = "GHCND",
  locationcategoryid = "CNTRY",
  limit = 1000
)

ncdc_data <- ncdc(
  token = noaa_api_key,
  datasetid = "GHCND",
  startdate = days_to_get[1],
  enddate = tail(days_to_get,1),
  locationid = "FIPS:01",
  limit = 1000,
  stationid = "GHCND:US1ALJF0064",
  add_units = T,
  datatypeid = "PRCP"
)

ncdc_data2 <- ncdc(
  token = noaa_api_key,
  datasetid = "GHCND",
  startdate = days_to_get[1],
  enddate = head(days_to_get,1),
  locationid = "FIPS:01",
  limit = 1000,
  stationid = "GHCND:US1ALJF0064",
  add_units = T
)

ncdc_data$data %>% 
  mutate(valueIn=(value/10)/25.4) %>% 
  View

ncdc_data$data %>% 
  mutate(valueIn=(value/10)/25.4) %>% 
  pull(valueIn) %>% sum

ncdc_data2$data %>% 
  View

ncdc_datasets(
  token = noaa_api_key
)

ncdc_datatypes(
  token = noaa_api_key,
  limit = 1000,
  datasetid = "GHCND"
) %>% 
  use_series(data) %>% 
  View

station1 <- ncdc_stations(
  token = noaa_api_key,
  stationid = "GHCND:AQW00061705"
)

al_stations_temp <- ncdc_stations(
  token = noaa_api_key,
  locationid = "FIPS:01",
  limit = 1000,
  datatypeid = "TMAX"
)

al_stations$data %>% 
  filter(maxdate==max(maxdate)) %>%  
  filter(between(latitude,33,33.75),between(longitude,-87,-86.5)) %>% 
  ggplot(mapping = aes(x=longitude,y=latitude))+
  geom_point()+
  geom_label_repel(aes(label = id))

al_temps <- ncdc(
  token = noaa_api_key,
  datasetid = "GHCND",
  startdate = days_to_get[1],
  enddate = head(days_to_get,1),
  locationid = "FIPS:01",
  limit = 1000,
  add_units = T,
  datatypeid = c("TMAX","TMIN","TOBS","TSUN")
)

al_temps %>% 
  use_series(data)

ncdc_data %>% 
  use_series(data) %>% 
  mutate(valueF=(value/10)*9/5+32) %>% 
  left_join(
    al_stations_temp %>% 
      use_series(data) %>% 
      select(name,elevation,longitude,latitude,id),
    by=c("station"="id")
  ) %>% 
  View

al_stations %>% 
  use_series(data) %>% 
  filter(id=="GHCND:USC00013655")

#### Files ####

noaa_state_codes <- read.xlsx("State Codes.xlsx",sheet = 1)

fips_county_codes <- "C:/Users/Sam/Downloads/all-geocodes-v2018.xlsx" %>% 
  read.xlsx(sheet = "v2018geocodes",startRow = 5)

nclim_pcpn <- "C:/Users/Sam/Downloads/climdiv-pcpncy-v1.0.0-20200406" %>% 
  read_delim(
    delim = " ",
    trim_ws = T,
    col_names = F
  ) %>% 
  select(X1:X13) %>% 
  setDT %>% 
  data.table::melt(
    id.vars = "X1",
    variable.factor = F,
    variable.name = "Month",
    value.name = "Value"
  ) %>% 
  mutate_at(vars(Month),~sub("^X","",.) %>% as.integer %>% subtract(1)) %>% 
  mutate(
    State_Code = substr(X1,1,2),
    County_Code = substr(X1,3,5),
    Element_Code = substr(X1,6,7),
    Year=substr(X1,8,11) %>% as.integer
  ) %>% 
  mutate(Date=paste(Year,Month,"01",sep = "-") %>% as.Date) %>% 
  filter(Value!=-9.99) %>% 
  mutate_at(vars(State_Code),as.integer) %>% 
  left_join(
    noaa_state_codes,
    by="State_Code"
  ) %>% 
  filter(Date>="1990-01-01") %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="050") %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary.Level=="040") %>% 
          select(`State.Code.(FIPS)`,State=`Area.Name.(including.legal/statistical.area.description)`),
        by="State.Code.(FIPS)"
      ) %>% 
      select(State,County_Code=`County.Code.(FIPS)`,County_Name=`Area.Name.(including.legal/statistical.area.description)`),
    by=c("State","County_Code")
  )

nclim_tmin <- "C:/Users/Sam/Downloads/climdiv-tmincy-v1.0.0-20200406" %>% 
  read_delim(
  delim = " ",
  trim_ws = T,
  col_names = F
) %>% 
  select(X1:X13) %>% 
  setDT %>% 
  data.table::melt(
    id.vars = "X1",
    variable.factor = F,
    variable.name = "Month",
    value.name = "Value"
  ) %>% 
  mutate_at(vars(Month),~sub("^X","",.) %>% as.integer %>% subtract(1)) %>% 
  mutate(
    State_Code = substr(X1,1,2),
    County_Code = substr(X1,3,5),
    Element_Code = substr(X1,6,7),
    Year=substr(X1,8,11) %>% as.integer
  ) %>% 
  mutate(Date=paste(Year,Month,"01",sep = "-") %>% as.Date) %>% 
  filter(Value!=-99.9) %>% 
  mutate_at(vars(State_Code),as.integer) %>% 
  left_join(
    noaa_state_codes,
    by="State_Code"
  ) %>% 
  filter(Date>="1990-01-01") %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="050") %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary.Level=="040") %>% 
          select(`State.Code.(FIPS)`,State=`Area.Name.(including.legal/statistical.area.description)`),
        by="State.Code.(FIPS)"
      ) %>% 
      select(State,County_Code=`County.Code.(FIPS)`,County_Name=`Area.Name.(including.legal/statistical.area.description)`),
    by=c("State","County_Code")
  )

nclim_tmax <- "C:/Users/Sam/Downloads/climdiv-tmaxcy-v1.0.0-20200406" %>% 
  read_delim(
    delim = " ",
    trim_ws = T,
    col_names = F
  ) %>% 
  select(X1:X13) %>% 
  setDT %>% 
  data.table::melt(
    id.vars = "X1",
    variable.factor = F,
    variable.name = "Month",
    value.name = "Value"
  ) %>% 
  mutate_at(vars(Month),~sub("^X","",.) %>% as.integer %>% subtract(1)) %>% 
  mutate(
    State_Code = substr(X1,1,2),
    County_Code = substr(X1,3,5),
    Element_Code = substr(X1,6,7),
    Year=substr(X1,8,11) %>% as.integer
  ) %>% 
  mutate(Date=paste(Year,Month,"01",sep = "-") %>% as.Date) %>% 
  filter(Value!=-99.9) %>% 
  mutate_at(vars(State_Code),as.integer) %>% 
  left_join(
    noaa_state_codes,
    by="State_Code"
  ) %>% 
  filter(Date>="1990-01-01") %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="050") %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary.Level=="040") %>% 
          select(`State.Code.(FIPS)`,State=`Area.Name.(including.legal/statistical.area.description)`),
        by="State.Code.(FIPS)"
      ) %>% 
      select(State,County_Code=`County.Code.(FIPS)`,County_Name=`Area.Name.(including.legal/statistical.area.description)`),
    by=c("State","County_Code")
  )

nclim_tmpc <- "C:/Users/Sam/Downloads/climdiv-tmpccy-v1.0.0-20200406" %>% 
  read_delim(
    delim = " ",
    trim_ws = T,
    col_names = F
  ) %>% 
  select(X1:X13) %>% 
  setDT %>% 
  data.table::melt(
    id.vars = "X1",
    variable.factor = F,
    variable.name = "Month",
    value.name = "Value"
  ) %>% 
  mutate_at(vars(Month),~sub("^X","",.) %>% as.integer %>% subtract(1)) %>% 
  mutate(
    State_Code = substr(X1,1,2),
    County_Code = substr(X1,3,5),
    Element_Code = substr(X1,6,7),
    Year=substr(X1,8,11) %>% as.integer
  ) %>% 
  mutate(Date=paste(Year,Month,"01",sep = "-") %>% as.Date) %>% 
  filter(Value!=-99.9) %>% 
  mutate_at(vars(State_Code),as.integer) %>% 
  left_join(
    noaa_state_codes,
    by="State_Code"
  ) %>% 
  filter(Date>="1990-01-01") %>% 
  left_join(
    fips_county_codes %>% 
      filter(Summary.Level=="050") %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary.Level=="040") %>% 
          select(`State.Code.(FIPS)`,State=`Area.Name.(including.legal/statistical.area.description)`),
        by="State.Code.(FIPS)"
      ) %>% 
      select(State,County_Code=`County.Code.(FIPS)`,County_Name=`Area.Name.(including.legal/statistical.area.description)`),
    by=c("State","County_Code")
  )

df <- nclim_tmpc %>% 
  filter(State=="Alabama",County_Name=="Jefferson County")

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
  select(Date=ds,Predicted=yhat,Trend=trend) %>% 
  setDT %>%
  melt(id="Date",measure.vars = c("Predicted","Trend"),variable.factor = F) %>%
  mutate_at(vars(Date),as.Date) %>%
  rbind(
    df %>%
      select(Date,value=Value) %>%
      mutate(variable="Observed")
  ) %>% 
  mutate_at(vars(variable),~factor(.,levels = c("Predicted","Observed","Trend"))) %>% 
  arrange(variable) %>% 
  ggplot(aes(x=Date,y=value,color=variable))+
  geom_line(lwd = 1.5,alpha = .9)+
  scale_x_date(date_breaks = "10 years",date_labels = "%Y")+
  scale_color_manual(values = c("Predicted"="red3","Observed"="deepskyblue3","Trend"="yellow2"))

forecast %>%
  mutate_at(vars(ds),as.Date) %>%
  mutate(Month=format(ds,"%m"),
         Monthtext=format(ds,"%B")) %>%
  group_by(Month,Monthtext) %>%
  summarise_at(vars(yhat),mean) %>%
  ungroup %>%
  mutate(`Per Month`=yhat-mean(yhat))

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
  )

### Temp ####

require(prophet)

df <- nclim_tmin %>% 
  filter(State=="South Dakota",County_Name=="Minnehaha County",Date>="2010-01-01")

m <- df %>%
  select(ds = Date, y = Value) %>%
  mutate(cap=.5) %>% 
  prophet(
    holidays = NULL,
    daily.seasonality = F
  )

forecast <- predict(m,make_future_dataframe(m,periods = 12,freq = "month"))

forecast %>%
  select(Date=ds,Predicted=yhat,Trend=trend) %>% 
  setDT %>%
  melt(id="Date",measure.vars = c("Predicted","Trend"),variable.factor = F) %>%
  mutate_at(vars(Date),as.Date) %>%
  rbind(
    df %>%
      select(Date,value=Value) %>%
      mutate(variable="Observed")
  ) %>% 
  ggplot(aes(x=Date,y=value,color=variable))+
  geom_line(lwd = 1.5)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

forecast %>%
  mutate_at(vars(ds),as.Date) %>%
  mutate(Month=format(ds,"%m"),
         Monthtext=format(ds,"%B")) %>%
  group_by(Month,Monthtext) %>%
  summarise_at(vars(yhat),mean) %>%
  ungroup %>%
  mutate(`Per Month`=yhat-mean(yhat))
