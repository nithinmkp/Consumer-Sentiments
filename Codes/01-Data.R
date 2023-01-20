
# Set-up ------------------------------------------------------------------

## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("readxl","tidyverse","fs","lubridate","writexl","rio","readr",
            "janitor")
package_fn(packages)

## Folders and Files ----
raw_data<-"Data/Consumer Sentiments/Raw Data"
clean_data<-"Data/Consumer Sentiments/Cleaned Data"
dir_create(clean_data)
#--------------------------------------------------------------------------#
# Data ----

## Reading the data ----
file_list<-list.files(here::here(raw_data),pattern = "*.csv")
file_list<-paste0(here::here(raw_data),"/",file_list)
dat_list<-map(file_list,data_import_fn) |> 
        set_names(str_extract(file_list,
                              pattern="[a-zA-Z]{3}_[a-zA-Z]{3,4}_[0-9]{4}"))

## Save Cleaned Data ----
clean_master_data<-paste0(clean_data,"/Master Data")
dir_create(clean_master_data)
out_file<-paste0(here::here(clean_master_data),"/",
                 list.files(here::here(raw_data),pattern = "*.csv"))

### Excel Files ----
walk2(dat_list,out_file,write_xlsx)

### Rdata files ----
saveRDS(dat_list,paste0(here::here(clean_master_data),"/","datlist.Rdata"))
