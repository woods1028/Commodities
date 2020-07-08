require(plyr);require(dplyr)
require(Quandl)
require(lubridate)
require(ggplot2)
require(scales)
require(countrycode)
require(data.table)
require(magrittr)
require(cowplot)
require(comtradr)
require(tidyUSDA)
require(rgeos)
require(soilDB)
require(dbplyr)
require(ggrepel)
require(stringr)
require(readr)
require(rnoaa)
require(readr)
require(openxlsx)
require(prophet)
require(beepr)
theme_update(
  axis.title.y = element_text(angle = 0,vjust = .5),axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),legend.text = element_text(size = 14),
  strip.text = element_text(size = 18),plot.title = element_text(size = 20, hjust = .5)
)

quandl_api_key <- "API Keys.xlsx" %>% 
  read.xlsx(sheet = 1) %>% 
  filter(Type=="Quandl") %>% 
  pull(Key)

Quandl.api_key(quandl_api_key)

mydata <- Quandl("FRED/GDP",type = "raw")

Corn <- Quandl("CHRIS/CME_C1")

Coffee <- Quandl("CHRIS/ICE_KC1")

Soybeans <- Quandl("CHRIS/CME_S1")

Rice <- Quandl("CHRIS/CME_RR1")

tickers <- "C:/Users/Sam/Downloads/continuous.csv" %>% 
  read.csv(stringsAsFactors = F)

tickers %>% 
  filter(regexpr("Corn",Name)>0)

list("Corn","Coffee","Soybeans","Rice") %>% 
  lapply(function(x) {
    x %>% 
      get %>% 
      filter(Date>=(Sys.Date()-years(20))) %>% 
      select(Date,Settle) %>% 
      mutate(Type=x)
  }) %>% 
  bind_rows %>% 
  ggplot(mapping = aes(x=Date,y=Settle,color = Type))+
  geom_line(lwd = 1.5)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  facet_grid(Type~.,scales = "free_y")+
  labs(y="Price",x="",color = "")+
  theme(
    strip.background = element_blank(),strip.text = element_blank()
  )

ct_commodity_lookup("Corn")

1005

us_corn <- ct_search(reporters = "USA",partners = "All",commod_codes = "1005",
                        start_date = "2015",end_date = "2019",freq = "annual")

us_corn %>% 
  filter(trade_flow=="Export") %>% 
  select(year,reporter_iso,partner_iso,commodity,qty_unit,qty_unit_code,qty,netweight_kg,trade_value_usd) %>% 
  head

us_corn %>% 
  filter(trade_flow=="Export",partner_iso!="WLD") %>% 
  group_by(year) %>% 
  mutate(Rank=frankv(netweight_kg,order = -1,ties.method = "min")) %>% 
  ungroup %>% 
  mutate_at(vars(partner_iso),~ifelse(Rank>7,"Others",.)) %>% 
  mutate(partner_iso=factor(partner_iso,levels = extract(partner_iso,partner_iso!="Others") %>% unique %>% c("Others"))) %>% 
  group_by(year,partner_iso) %>% 
  summarise_at(vars(netweight_kg),sum) %>% 
  ungroup %>% 
  ggplot(mapping = aes(x = year,y = netweight_kg, color = partner_iso))+
  geom_line(lwd = 1.5)+
  geom_point(size = 3)

us_corn %>% 
  filter(partner_iso!="WLD") %>% 
  mutate(
    Region=countrycode(partner_iso,origin = "iso3c",destination = "region"),
    Continent=countrycode(partner_iso,origin = "iso3c",destination = "continent")
  ) %>% 
  mutate(
    Area=ifelse(Continent %in% c("Europe","Africa","Oceania","Asia"),Continent,Region)
  ) %>% 
  group_by(year,Area) %>% 
  summarise_at(vars(netweight_kg),sum) %>% 
  ungroup %>% 
  list(
    group_by(.,Area) %>% 
      summarise_at(vars(netweight_kg),sum) %>% 
      ungroup %>% 
      mutate(Rank=frankv(netweight_kg,order = -1,ties.method = "min")) %>% 
      select(Area,Rank)
  ) %>% 
  join_all(by="Area") %>% 
  arrange(Rank) %>% 
  mutate(Area=factor(Area,levels = unique(Area))) %>% 
  ggplot(mapping = aes(x=year,y=netweight_kg,color=Area))+
  geom_line(lwd = 1.5)+
  geom_point(size = 3)

psd_api_key <- "4C2554AB-EC7F-4F25-B1DE-47332EAD4F48"

acreage_state <- c("CORN","SOYBEANS") %>% 
  lapply(function(x) {
    getQuickstat(
      key = nass_api_key,
      program = "SURVEY",
      sector = "CROPS",
      group = "FIELD CROPS",
      commodity = x,
      category = "AREA PLANTED",
      data_item = paste(x,"ACRES PLANTED",sep = " - "),
      geographic_level = "STATE"
    )
  }) %>% 
  bind_rows

production_state <- c("CORN","SOYBEANS") %>% 
  lapply(function(x) {
    getQuickstat(
      key = nass_api_key,
      program = "SURVEY",
      sector = "CROPS",
      group = "FIELD CROPS",
      commodity = x,
      category = "PRODUCTION",
      geographic_level = "STATE"
    )
  }) %>% 
  bind_rows

acreage_state %>% 
  filter(year>2000) %>% 
  filter(reference_period_desc=="YEAR") %>%
  filter(state_alpha %in% c("IA","IL","IN","NE","TN","GA","TX")) %>% 
  ggplot(mapping = aes(x=year,y=Value,color=state_alpha))+
  geom_line(lwd = 1.5)+
  geom_point(size = 2)+
  facet_wrap(~commodity_desc)+
  scale_y_continuous(labels = comma)+
  labs(y="Acres",x="",color = "")

acreage_state %>% 
  filter(year>2000) %>% 
  filter(reference_period_desc=="YEAR") %>%
  filter(state_alpha %in% c("IA","IL","IN","NE","TN","GA","TX")) %>% 
  left_join(
    production_state %>% 
      filter(
        regexpr("IRRIGATED",short_desc)<0,
        regexpr("FOLLOWING",short_desc)<0,
        regexpr("IN BU",short_desc)>0
      ) %>% 
      filter(reference_period_desc=="YEAR") %>% 
      select(reference_period_desc,year,commodity_desc,Bushels=Value,state_alpha),
    by=c("commodity_desc","year","reference_period_desc","state_alpha")
  ) %>% 
  mutate(Yield=Bushels/Value) %>% 
  ggplot(mapping = aes(x=year,y=Yield,color=state_alpha))+
  geom_line(lwd = 1.5)+
  geom_point(size = 2)+
  facet_wrap(~commodity_desc)+
  scale_y_continuous(labels = comma)+
  labs(y="Yield",x="",color = "")

psd <- "C:/Users/Sam/Downloads/psd_alldata.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  filter(Commodity_Description %in% c("Corn","Rice, Milled","Cotton","Oil, Soybean","Meal, Soybean","Wheat","Coffee, Green"))

psd1 <- psd %>% 
  filter(Attribute_Description=="Production") %>% 
  mutate_at(vars(Commodity_Description),~ifelse(regexpr("Soybean",.)>0,"Soybean",.)) %>% 
  group_by(Commodity_Description,Country_Code,Country_Name,Market_Year,Unit_Description) %>% 
  summarise_at(vars(Value),sum) %>% 
  ungroup 
  
psd1 %>% 
  pull(Commodity_Description) %>% 
  unique %>% 
  lapply(function(x) {
    psd1  %>% 
      filter(Commodity_Description==x) %>% 
      filter(!(Country_Code %in% c("E2","E4"))) %>% 
      group_by(Country_Code,Country_Name,Market_Year,Unit_Description) %>% 
      summarise_at(vars(Value),sum) %>% 
      filter(Market_Year %in% 2009:2019) %>% 
      group_by(Market_Year) %>% 
      mutate(
        Pct=Value/sum(Value),
        Rank=frankv(Pct,order = -1,ties.method = "min"),
        Pseudo=ifelse(Rank>7,"Others",Country_Name)
      ) %>% 
      ungroup %>% +
      mutate(
        Region=countrycode(Country_Code,origin = "fips",destination = "region"),
        Continent=countrycode(Country_Code,origin = "fips",destination = "continent"),
        Area=ifelse(Continent %in% c("Europe","Africa","Oceania","Asia"),Continent,Region)
      ) %>% 
      # group_by(Area,Market_Year) %>% 
      # summarise_at(vars(Pct),sum) %>% 
      # ungroup %>% 
      group_by(Pseudo,Market_Year) %>%
      summarise_at(vars(Pct),sum) %>%
      ungroup %>%
      arrange(-Pct) %>% 
      mutate(Pseudo=factor(Pseudo,levels = (
        extract(Pseudo,Pseudo!="Others") %>%
          unique %>%
          c("Others")
      ))) %>%
      { ggplot(.,mapping = aes(x = Market_Year,y=Pct,color=Pseudo))+
          geom_line(lwd = 1.5)+
          geom_point(size = 3)+
          labs(x="",color="",y="")+
          scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,max(.$Pct)))+
          scale_x_continuous(breaks = seq(2009,2019,by = 2))+
          ggtitle(x)+
          scale_color_manual(
            values = c("Argentina"="deepskyblue1","Brazil" = "green3","China"="red","India"="orange","Canada"="red4",
                       "Mexico"="firebrick3","Ukraine"="yellow3","United States"="blue","Others"="gray40","Paraguay"="navy",
                       "Japan"="slategray3","Russia"="dodgerblue","Bangladesh"="darkgreen","Burma"="lawngreen","Indonesia"="darkred",
                       "Thailand"="midnightblue","Vietnam"="goldenrod1","Australia"="yellow","Pakistan"="seagreen4",
                       "Turkey"="indianred1","Uzbekistan"="steelblue1","South Africa"="forestgreen",
                       "Colombia"="yellow3","Honduras"="royalblue","Ethiopia"="darkolivegreen4","Uganda"="black","Peru"="deeppink")
          )
      }
  }) %>% 
  align_plots(
    plotlist = .,
    align = "hv"
  ) %>% 
  plot_grid(
    plotlist = .
  )

psd %>%
  filter(Attribute_Description %in% c("Yield","Area Harvested")) %>% 
  filter(Market_Year==2019) %>% 
  filter(Commodity_Description=="Cotton") %>% 
  mutate_at(vars(Value),~ifelse(Attribute_Description=="Yield",
                 ifelse(Unit_Description=="(MT/HA)",.*1000*2.2/(56*2.47),.*2.2/(56*2.47)),
                 .*1000*2.47)) %>% 
  dcast(Country_Name~Attribute_Description,value.var="Value") %>% 
  View
