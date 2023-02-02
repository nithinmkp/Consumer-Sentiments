
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions.R"))

## Packages ----
packages<-c("tidyverse","fs","writexl","here")
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
dat_list_gen<-dat_list |> 
        reduce(bind_rows) |> 
        data_transform_fn(pivot_cols = 10:14)

### Category-Wise ICS ----

#### Data Transformation ----

cat_dat_list<-dat_list |> 
        reduce(bind_rows) |> 
        group_vars_fn(var_select = c(6,15:20),
                      group_var = Series)

### State-Wise ICS ----

#### Data Transformation ----

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


#----------------------------------------------------------------------------------#
# Write Data ----

## General ICS ----
### Excel File ----
write_xlsx(dat_ICS,here(clean_data,"ICS.xlsx"))

### Rdata ----
saveRDS(dat_ICS,here(clean_data,"ICS.Rdata"))

## Category Wise ICS ----
### Excel File ----
#### Renaming for sheet names
names(lst_dat_ICS$age_group)[c(2,8)]<-c("Balanced households-NO Seniors",
                                        "Balanced households-Seniors")
names(lst_dat_ICS$occupation_group)[c(1,2,16,13,19)]<-c("White-collar Clers",
                                                        "White-collar Pro",
                                                        "Non-industrial Techs",
                                                        "Qualified Self-employed",
                                                        "Scoial-worker/activisit")
names(lst_dat_ICS$education_group)[c(3,5,11)]<-c("Matriculates-minor-household",
                                                 "Matriculates-major-household",
                                                 "Matri-dominated-household")

tibble(x=lst_dat_ICS,
       file=here(clean_master_data,paste0(names(cat_dat_list),".xlsx"))) |> 
        pwalk(write.xlsx)

### Rdata ----
saveRDS(lst_dat_ICS,here(clean_data,"Cat-ICS.Rdata"))

## State-Wise ICS ----
### Excel File ----
write_xlsx(lst_state_ICS,here(clean_data,"state-ICS.xlsx"))

### Rdata ----
saveRDS(lst_state_ICS,here(clean_data,"state-ICS.Rdata"))
#----------------------------------------------------------------------------------#

