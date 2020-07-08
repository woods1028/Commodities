require(tidyr,exclude = "extract")
require(broom)

my_spdf <- "C:/Users/Sam/Downloads/tl_2017/tl_2017_us_county.shp" %>% 
  readOGR

plot(my_spdf)

spdf_fortified <- tidy(my_spdf, region = "GEOID")

new_spdf <- spdf_fortified %>% 
  left_join(
    my_spdf@data %>% 
      select(id=GEOID,COUNTYFP,STATEFP) %>% 
      mutate_at(vars(id,COUNTYFP,STATEFP),as.character),
    by="id"
  ) %>% 
  left_join(
    tmpc_df %>% 
      select(-state_code),
    by=c("STATEFP"="state_fips_code","COUNTYFP"="county_code")
  )

ggplot() +
  geom_polygon(data = new_spdf, aes( x = long, y = lat, group = group, fill = per_year),color = "black")+
  coord_cartesian(ylim=c(25,50),xlim=c(-125,-65))

new_spdf2 <- spdf_fortified %>% 
  left_join(
    my_spdf@data %>% 
      select(id=GEOID,COUNTYFP,STATEFP) %>% 
      mutate_at(vars(id,COUNTYFP,STATEFP),as.character),
    by="id"
  ) %>% 
  left_join(
    pcpn_df %>% 
      select(-state_code),
    by=c("STATEFP"="state_fips_code","COUNTYFP"="county_code")
  )

ggplot() +
  geom_polygon(data = new_spdf2,aes( x = long, y = lat, group = group, fill = per_year),color = "black")+
  coord_cartesian(ylim=c(25,50),xlim=c(-125,-65))
