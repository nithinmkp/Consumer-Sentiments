
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions_3.R"))

## Packages ----
packages<-c("tidyverse","fs","writexl","readr","janitor","here","rio",
            "readxl","lubridate","rlist","tabulizer","magrittr","renv")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
income_path<-here(raw_data,"income_data")
clean_data<-here("Data","Cleaned Data")
#--------------------------------------------------------------------------#

# Data ----
# zip_files<-list.files(income_path,"*.zip",full.names = T)
# walk(zip_files,unzip,exdir=income_path)
files<-list.files(income_path,"*.csv$",full.names = T)
cols<-c("HH_ID","MONTH","STATE","DISTRICT","REGION_TYPE","TOTAL_INCOME")
dat_list<-map(files,read_fn,
              na_char="-99",
              colum_select=cols)
dat_list<-dat_list |> 
        list.rbind()
save(dat_list,file = here(clean_data,"income_data.RData"))
