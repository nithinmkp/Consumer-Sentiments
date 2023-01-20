
# Set-up ------------------------------------------------------------------

## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("readxl","tidyverse","fs","lubridate","writexl","rio","readr")
package_fn(packages)

## Folders ad Files ----
raw_data<-"Data/Consumer Sentiments/Raw Data"
clean_data<-"Data/Consumer Sentiments/Cleaned Data"
dir_create(clean_data)
clean_master_data<-paste0(clean_data,"/Master Data")

#--------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#

# Data ----
## Load Data ----
dat_list<-readRDS(paste0(here::here(clean_master_data),"/","datlist.Rdata")) # Master Data

## Data Transformation ----
dat_list<-map(dat_list,data_transform_fn)

#----------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#
# Calculation of ICS ----
## Components of ICS and ICS ----
dat_ICS<-map(dat_list,ind_calculate_fn)|> 
        map_df(bind_rows) |> 
        arrange(month_slot) |> 
        rowwise() |> 
        mutate(ICS=mean(c_across(2:6)))


#----------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------#
# Write Data ----
## Excel File ----
write_xlsx(dat_ICS,paste0(here::here(clean_data),
                          "/","ICS.xlsx"))

## Rdata ----
saveRDS(dat_ICS,paste0(here::here(clean_data),
                       "/","ICS.Rdata"))
