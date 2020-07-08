require(plyr);require(dplyr)
require(ggplot2)
require(data.table)
theme_update(
  axis.title = element_text(size = 14),axis.title.y = element_text(angle = 0,vjust = .5),
  axis.text = element_text(size = 12),legend.text = element_text(size = 12)
)

"https://www.nasdaq.com/market-activity/commodities/zc/historical"

corn <- "C:/Users/Sam/Downloads/HistoricalQuotes (2).csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate_at(vars(Date),~as.Date(.,format = "%m/%d/%Y"))

coffee <- "C:/Users/Sam/Downloads/HistoricalQuotes (3).csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate_at(vars(Date),~as.Date(.,format = "%m/%d/%Y"))

corn %>% 
  ggplot(mapping = aes(x = Date,y = Close.Last))+
  geom_line(lwd = 1.5)

corn %>% 
  setDT %>% 
  melt(
    id.vars = c("Date"),
    measure.vars = c("High","Low"),
    variable.factor = F,
    variable.name = "Type",
    value.name = "Price"
  ) %>% 
  ggplot(mapping = aes(x=Date,y=Price,color = Type))+
  geom_line(lwd = 1.5,alpha = .7)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(y="Price",x="",color="")

corn %>% 
  mutate(Type="Corn") %>% 
  bind_rows(
    coffee %>% 
      mutate(Type="Coffee") %>% 
      mutate_at(vars(Close.Last),~.*100)
  ) %>% 
  ggplot(mapping = aes(x=Date,y=Close.Last,color=Type))+
  geom_line(lwd = 1.5)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(y="Price",x="",color="")
