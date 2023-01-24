
# Set-up ------------------------------------------------------------------

## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("tidyverse","fs","writexl")
package_fn(packages)

## Folders ad Files ----
raw_data<-here("Data","Raw Data")
clean_data<-here("Data","Cleaned Data")
dir_create(clean_data)
clean_master_data<-here("Data","Cleaned Data","Master Data")

#--------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#

# Data ----
## Load Data ----
dat_list<-readRDS(here(clean_master_data,"datlist.Rdata")) # Master Data

### General ICS ----

#### Data Transformation ----
dat_list_gen<-map(dat_list,data_transform_fn,
              pivot_cols = 10:14)

### Category-Wise ICS ----

#### Data Transformation ----

cat_dat_list<-dat_list |> 
        reduce(bind_rows) |> 
        group_vars_fn(var_select = c(6,15:20),
                      group_var = Series)

#----------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#
# Calculation of ICS ----

## General ICS ----
### Components of ICS and ICS ----
dat_ICS<-map(dat_list_gen,ind_calculate_fn)|> 
        map_df(bind_rows) |> 
        arrange(month_slot) |> 
        rowwise() |> 
        mutate(ICS=mean(c_across(2:6)))


## Category Wise ICS
### Components of ICS and ICS ----
cat_dat_list<-map(cat_dat_list,~.x %>%
                          group_split_fn()) %>% 
        map(.,~.x %>%
                    map(.,~.x %>% 
                                data_transform_fn(pivot_cols=9:13)))
lst_dat_ICS<-map(cat_dat_list,~.x %>%
                         map(.,~.x %>%
                                     ind_calculate_fn))

#----------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------#
# Write Data ----

## General ICS ----
### Excel File ----
write_xlsx(dat_ICS,here(clean_data,"ICS.xlsx"))

### Rdata ----
saveRDS(dat_ICS,here(clean_data,"ICS.Rdata"))

## Category Wise ICS ----
### Excel File ----


### Rdata ----

#----------------------------------------------------------------------------------#

