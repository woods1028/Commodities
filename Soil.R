require(soilDB)

soil_profile <- function(state_name,county_code) {
  
  state_code <- fips_county_codes %>% 
    rename(
      State_Code=`State Code (FIPS)`,
      Area=`Area Name (including legal/statistical area description)`
    ) %>% 
    filter(toupper(Area)==toupper(state_name)) %>% 
    pull(State_Code)
  
  state_abbrev <- state.abb %>% 
    data.frame(
      State_Abb=.,
      State_Name=state.name,
      stringsAsFactors = F
    ) %>% 
    filter(State_Name==state_name) %>% 
    pull(State_Abb)
  
  county_code_query <- paste(state_abbrev,county_code,sep = "")

  query <- paste(
    "SELECT 
    component.mukey, component.cokey, comppct_r, compname, taxclname, 
    taxorder, taxsuborder, taxgrtgroup, taxsubgrp,
    chorizon.cec7_l,chorizon.cec7_r,chorizon.cec7_h,
    chorizon.hzname,chorizon.hzdept_r,chorizon.hzdepb_r,ph1to1h2o_r
    FROM legend
    INNER JOIN mapunit ON mapunit.lkey = legend.lkey
    LEFT OUTER JOIN component ON component.mukey = mapunit.mukey
    LEFT JOIN chorizon ON component.cokey = chorizon.cokey
    WHERE legend.areasymbol = '",
    county_code_query,
    "'",
    sep = ""
  )
  
  result <- SDA_query(query) %>% 
    suppressMessages
    
  if (is.null(result)==T) {
    return(result)
  }
  
  result %>% 
    tibble %>% 
    mutate(
      state_code=state_code,
      county_code=county_code,
      state_name=state_name
    )
  
}

state_soil_profile <- function(state_name) {
  
  fips_state_code <- fips_county_codes %>% 
    filter(`Summary Level`=="040",`Area Name (including legal/statistical area description)`==state_name) %>% 
    pull(`State Code (FIPS)`)
  
  counties_state <- fips_county_codes %>% 
    filter(`Summary Level`=="050",`State Code (FIPS)`==fips_state_code) %>% 
    pull(`County Code (FIPS)`)
  
  counties_state %>% 
    lapply(function(x) {
      soil_profile(state_name = state_name,county_code = x)
    }) %>% 
    bind_rows
  
}

dirt <- state.name %>% 
  lapply(function(x) {
    print(x) 
    state_number <- match(x,state.name)
    if (state_number>1 & state_number%%5==1) {
      print("Waiting 1 min...")
      Sys.sleep(60)
    }
    state_soil_profile(x) %>% 
      suppressWarnings
  }) %>% 
  bind_rows

depth_buckets <- c(0,6,12,30,50,70,100)

dirt_by_county <- dirt %>% 
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
  pivot_wider(id_cols = c(state_code,county_code),names_from = Depth_Bucket,values_from = c(CEC,PH))

dirt_by_county %>% 
  write_excel_csv("Dirt By County.csv")
