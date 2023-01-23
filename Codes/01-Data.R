
# Set-up ------------------------------------------------------------------

## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("tidyverse","fs","writexl","readr","janitor","here","labelled")
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

## labels for variables
description <-tibble(name=names(dat_list$jan_apr_2016)[10:14],
       label=c("Current Financial conditions compared with last 12 months",
               "Expected Financial conditions in next 12 months",
               "Expected business condtions in next 12 months",
               "Expected business condtions in 5 years",
               "Buying Conditions"))
var_labels <- setNames(as.list(description$label), description$name)

dat_list<-map(dat_list,~.x |> 
                      set_variable_labels(.labels = var_labels,
                                          .strict = F))
## Save Cleaned Data ----
clean_master_data<-here(clean_data,"Master Data")
dir_create(clean_master_data)
out_file<-here(clean_master_data, file_list)
### Excel Files ----
walk2(dat_list,out_file,write_xlsx)

### Rdata files ----
saveRDS(dat_list,here(clean_master_data,"datlist.Rdata"))
