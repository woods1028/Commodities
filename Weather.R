require(tidyverse)
require(readxl)
require(magrittr)
require(RSelenium)
require(rvest)
require(beepr)

require(sparklyr)
options(spark.install.dir = "C:\\Spark")
options(sparklyr.log.console = TRUE) 

source("C:/Users/Sam Woods/Dropbox/Environment Calls.R")

sc <- spark_connect(master = "local", version = "3.0",log = "console")

#### get nClimDiv file ####

chrome_version <- function() {

  system2(command = "wmic",
          args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
          stdout = TRUE,
          stderr = TRUE) %>%
    stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(
      string =
        binman::list_versions(appname = "chromedriver") %>%
        dplyr::last()
    ) %>%
    as.numeric_version() %>%
    max() %>%
    as.character()
}

renew_chrome_port <- function() {
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
}

nClimDiv_file <- function() {
  
  rD <- rsDriver(browser="chrome", port=4570L, verbose=F,chromever = chrome_version())
  
  remDr <- rD[["client"]]
  
  base_url <- "https://www.ncei.noaa.gov/data/climdiv/archive/"
  
  remDr$navigate(base_url)
  
  Sys.sleep(3)
  
  ncei_html <- remDr$getPageSource()[[1]] %>% 
    read_html
  
  file_name <- ncei_html %>% 
    html_element("tr:nth-child(4) a") %>% 
    html_attr("href")
  
  file_link <- file_name %>% 
    paste(base_url,.,sep = "")
  
  upload_date <- file_link %>% 
    str_extract("\\d{8}") %>% 
    as.Date(format = "%Y%m%d")
  
  dest_folder <- "C:/Users/Sam Woods/Downloads"
  
  current_file <- grep(
    "Monthly-US-Climate-Divisions.+\\.gz$",
    list.files("C:/Users/Sam Woods/Downloads/",full.names = T),
    value = T
  )
  
  if (length(current_file)>0) {
  
  current_file_upload_date <- str_extract(current_file,"\\d{8}\\.tar") %>% 
    sub("\\.tar","",.) %>% 
    as.Date(format = "%Y%m%d")
  
  } else {
    current_file_upload_date <- as.Date("2010-01-01")
  }
  
  nClimDiv_path <- "C:/Users/Sam Woods/Downloads/Monthly nClimDiv Files"
  
  if (current_file_upload_date < upload_date ) {
   
    webElem <- remDr$findElement(using = 'css selector',"tr:nth-child(4) a")
    webElem$clickElement() 
    
    print("New file found.")
    
    Sys.sleep(180)
    
    file.remove(current_file)
    unlink(nClimDiv_path,recursive = T)
    
    file_path <- grep(file_name,list.files("C:/Users/Sam Woods/Downloads",full.names = T),value = T)
    
    untar(file_path,exdir = nClimDiv_path)
     
  } else {
    
    print("No new file.")
    
  }
  
  rD$server$stop()
  remDr$close()
  rm(rD,remDr)
  
}

#### Process ####

nClimDiv_file()

noaa_state_codes <- read_excel("State Codes.xlsx",sheet = 1)

fips_county_codes <- "all-geocodes-v2018.xlsx" %>% 
  read_excel(sheet = "v2018geocodes",skip = 4)

nClimDiv_path <- "C:/Users/Sam Woods/Downloads/Monthly nClimDiv Files"

noaa_state_codes <- copy_to(sc,df = noaa_state_codes)
fips_county_codes <- copy_to(sc,df = fips_county_codes)

col_declarations <- 1:13 %>% 
  paste("V",.,sep = "") %>% 
  list(
    c("character",rep("double",12))
  ) %>% 
  pmap(function(x,y) setNames(y,x)) %>% 
  unlist

nClimDiv <- c("pcpncy","tmpccy","tmincy","tmaxcy") %>% 
  map(
    ~grep(
      .x,
      list.files(
        nClimDiv_path,
        full.names = T
      ),
      value = T
    ) %>% 
      spark_read_csv(
        sc, .x, path = ., delimiter = "  ",trimws = T,header = F,memory = F,
        columns = col_declarations
      ) %>% 
      setNames(c("id",paste("Month",seq(as.Date("2020-01-01"),as.Date("2020-12-01"),by = "1 month") %>% format("%m"),sep = ""))) %>% 
      mutate(
        State_Code = substr(id,1,2),
        County_Code = substr(id,3,5),
        Element_Code = substr(id,6,7),
        Year=substr(id,8,11)
      ) %>% 
      pivot_longer(cols = matches("\\d$"),names_to = "Month",values_to = "Value") %>% 
      mutate_at(vars(Month),~regexp_replace(.,"Month","")) %>% 
      mutate(Date=as.Date(paste(Year,Month,"01",sep = "-"))) %>% 
      filter(Value!=-9.99) %>% 
      mutate_at(vars(State_Code),as.integer) %>%
      left_join(
        noaa_state_codes,
        by="State_Code"
      ) %>% 
      filter(Date>="1990-01-01") %>% 
      left_join(
        fips_county_codes %>% 
          filter(Summary_Level=="050") %>% 
          left_join(
            fips_county_codes %>% 
              filter(Summary_Level=="040") %>% 
              select(State_Code_FIPS,State=Area_Name_including_legalstatistical_area_description),
            by="State_Code_FIPS"
          ) %>% 
          select(State,County_Code=County_Code_FIPS,County_Name=Area_Name_including_legalstatistical_area_description),
        by=c("State","County_Code")
      ) %>% 
      mutate(dataset=.x)
  ) %>% 
  sdf_bind_rows

beep(sound = "coin")
