require(tidyverse)
require(readxl)
require(magrittr)
require(RSelenium)
require(rvest)

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
  
  dest_folder <- "C:/Users/Sam/Downloads"
  
  current_file <- grep(
    "Monthly-US-Climate-Divisions.+\\.gz$",
    list.files("C:/Users/Sam/Downloads/",full.names = T),
    value = T
  )
  
  current_file_upload_date <- str_extract(current_file,"\\d{8}\\.tar") %>% 
    sub("\\.tar","",.) %>% 
    as.Date(format = "%Y%m%d")
  
  nClimDiv_path <- "C:/Users/Sam/Downloads/Monthly nClimDiv Files"
  
  if (current_file_upload_date < upload_date) {
   
    webElem <- remDr$findElement(using = 'css selector',"tr:nth-child(4) a")
    webElem$clickElement() 
    
    print("New file found.")
    
    Sys.sleep(180)
    
    file.remove(current_file)
    unlink(nClimDiv_path,recursive = T)
    
    file_path <- grep(file_name,list.files("C:/Users/Sam/Downloads",full.names = T),value = T)
    
    untar(file_name,exdir = nClimDiv_path)
     
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

fips_county_codes <- "C:/Users/Sam/Downloads/all-geocodes-v2018.xlsx" %>% 
  read_excel(sheet = "v2018geocodes",skip = 4)

nClimDiv_path <- "C:/Users/Sam/Downloads/Monthly nClimDiv Files"

nClimDiv <- c("pcpncy","tmpccy","tmincy","tmaxcy") %>% 
  map(
    ~grep(
      .x,
      list.files(
        nClimDiv_path <- "C:/Users/Sam/Downloads/Monthly nClimDiv Files",
        full.names = T
      ),
      value = T
    ) %>% 
      read_delim(
        delim = " ",
        trim_ws = T,
        col_names = F
      ) %>% 
      select(X1:X13) %>% 
      pivot_longer(cols = X2:X13,names_to = "Month",values_to = "Value") %>% 
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
          filter(`Summary Level`=="050") %>% 
          left_join(
            fips_county_codes %>% 
              filter(`Summary Level`=="040") %>% 
              select(`State Code (FIPS)`,State=`Area Name (including legal/statistical area description)`),
            by="State Code (FIPS)"
          ) %>% 
          select(State,County_Code=`County Code (FIPS)`,County_Name=`Area Name (including legal/statistical area description)`),
        by=c("State","County_Code")
      )
  ) %>% 
  setNames(c("pcpncy","tmpccy","tmincy","tmaxcy"))
