
# Set-up ------------------------------------------------------------------

## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("tidyverse","fs","writexl","readr","janitor","here")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
clean_data<-here("Data","Cleaned Data")
dir_create(clean_data)
#--------------------------------------------------------------------------#
# Data ----

## Reading the data ----
file_list<-list.files(raw_data,pattern = "*.csv")
dat_list<-map(here(raw_data,file_list),data_import_fn,
              num_cols=1:9,
              char_cols=c(
                      FAMILY_FINANCES_COMPARED_TO_YEAR_AGO,
                      FAMILY_FINANCES_A_YEAR_LATER,
                      CONDITIONS_IN_COUNTRY_OVER_NEXT_12_MONTHS,
                      CONDITIONS_IN_COUNTRY_OVER_NEXT_5_YEARS,
                      IS_THIS_GOOD_TIME_TO_BUY_CONSUMER_DURABLES
              )) |> 
        set_names(str_extract(file_list,
                              pattern="[a-zA-Z]{3}_[a-zA-Z]{3,4}_[0-9]{4}"))

## Save Cleaned Data ----
clean_master_data<-here(clean_data,"Master Data")
dir_create(clean_master_data)
out_file<-here(clean_master_data, file_list)
### Excel Files ----
walk2(dat_list,out_file,write_xlsx)

### Rdata files ----
saveRDS(dat_list,here(clean_master_data,"datlist.Rdata"))
