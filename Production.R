require(tidyUSDA)
require(keyring)

nass_api_key <- key_get(service = "NASS")

states_to_get <- c("IOWA","NEBRASKA","ILLINOIS","SOUTH DAKOTA","INDIANA","OHIO") %>% toupper

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
      state = states_to_get
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
      state = states_to_get %>% .[1:(length(.)/2)]
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
      state = states_to_get %>% .[((length(.)/2)+1):length(.)]
    )
  }) %>% 
  bind_rows
