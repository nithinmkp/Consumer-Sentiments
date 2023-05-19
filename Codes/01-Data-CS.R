
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions.R"))

## Packages ----
packages<-c("tidyverse","fs","writexl","readr","janitor","here","labelled")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
aspirational_path<-here(raw_data,"aspirational")
clean_data<-here("Data","Cleaned Data")
dir_create(clean_data)
#--------------------------------------------------------------------------#
# Data ----

## Reading the data ----
file_list<-list.files(aspirational_path,pattern = "_aspi")
dat_list<-map(here(aspirational_path,file_list),data_import_fn,
              num_cols=c(1:9),
              char_cols=c(
                      FAMILY_FINANCES_COMPARED_TO_YEAR_AGO,
                      FAMILY_FINANCES_A_YEAR_LATER,
                      CONDITIONS_IN_COUNTRY_OVER_NEXT_12_MONTHS,
                      CONDITIONS_IN_COUNTRY_OVER_NEXT_5_YEARS,
                      IS_THIS_GOOD_TIME_TO_BUY_CONSUMER_DURABLES,
                      AGE_GROUP, INCOME_GROUP, OCCUPATION_GROUP,
                      EDUCATION_GROUP, GENDER_GROUP, SIZE_GROUP)) |> 
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
# out_file<-here(clean_master_data, file_list)
### Excel Files ----
# walk2(dat_list,out_file,write_xlsx)

### Rdata files ----
save(dat_list,file=here(clean_master_data,"datlist.RData"))

#--------------------------------------------------------------------------#
# Wrangling ----
## Data ----
base::load(file=here(clean_master_data,"datlist.RData"))

## General ICS ----

### Data Transformation ----
dat_list_gen<-dat_list |> 
        reduce(bind_rows) |> 
        data_transform_fn(pivot_cols = 10:14)

## Category-Wise ICS ----

### Data Transformation ----

cat_dat_list<-dat_list |> 
        reduce(bind_rows) |> 
        group_vars_fn(var_select = c(6,15:20),
                      group_var = Series)

## State-Wise ICS ----

### Data Transformation ----

state_dat_list<-dat_list |> 
        reduce(bind_rows)|> 
        group_vars_fn(var_select = c(3),
                      group_var = Series) 


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
# Calculation of ICS ----

## General ICS ----
### Components of ICS and ICS ----
dat_ICS<-dat_list_gen |>
        ind_calculate_fn() |> 
        rowwise() |> 
        mutate(ICS=mean(c_across(2:6)))


## Category Wise ICS ----
### Components of ICS and ICS ----
cat_dat_list<-map(cat_dat_list,~.x %>%
                          group_split_fn()) %>% 
        map(.,~.x %>%
                    map(.,~.x %>% 
                                data_transform_fn(pivot_cols=9:13)))
lst_dat_ICS<-map(cat_dat_list,~.x %>%
                         map(.,~.x %>%
                                     ind_calculate_fn))%>% 
        map(.,~.x %>% 
                    map(.,~.x %>% 
                                rowwise() |> 
                                mutate(ICS=mean(c_across(2:6)))))


## State-Wise ICS ----
### Components of ICS and ICS ----
state_dat_list<-map(state_dat_list,~.x %>%
                            group_split_fn()) %>% 
        map(.,~.x %>%
                    map(.,~.x %>% 
                                data_transform_fn(pivot_cols=9:13)))
lst_state_ICS<-map(state_dat_list,~.x %>%
                           map(.,~.x %>%
                                       ind_calculate_fn))%>% 
        flatten() %>%
        map(.,~.x %>% 
                    rowwise() |> 
                    mutate(ICS=mean(c_across(2:6))))

#----------------------------------------------------------------------------------#
# Save Files ----
save(dat_ICS,file=here(clean_data,"ICS.RData"))
save(lst_dat_ICS,file=here(clean_data,"Cat-ICS.RData"))
save(lst_state_ICS,file=here(clean_data,"state-ICS.RData"))
