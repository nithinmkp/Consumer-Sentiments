
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions_2.R"))

## Packages ----
packages<-c("tidyverse","fs","writexl","readr","janitor","here","rio",
            "readxl","lubridate","rlist","tabulizer","magrittr","renv")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
consumption_path<-here(raw_data,"consumption")
clean_data<-here("Data","Cleaned Data")
#--------------------------------------------------------------------------#

# Data ----
base::load(file=here(clean_data,"consumption_full_data.RData"))
col_file<-import_list(here(raw_data,"cmie_vars_MASTER.xlsx"))[3:4]
labels_food<-map(1:2,~col_file$Food |>
                    dplyr::select(all_of(.x)) |>
                    distinct() |>
                    pull()) |>
        set_names(colnames(col_file$Food)[1:2])
labels_foodndfuel<-map(1:2,~col_file$`Food&fuel` |>
                         dplyr::select(all_of(.x)) |>
                         distinct() |>
                         pull()) |>
        set_names(colnames(col_file$`Food&fuel`)[1:2])


## Calculate Total Expenditure for different groups ----

### Food

datlist_exp_food<-map(datlist,~.x %>%
                         map(labels_food$Group,
                             total_exp_fn,dat=.,vars_df=col_file$Food) |>
                         reduce(left_join)) |>
        map(~.x %>%
                    mutate("Total_Exp"=rowSums(dplyr::select(., starts_with("TOT_",ignore.case=F)),
                                               na.rm = TRUE))) |>
        map(~.x |>
                    mutate(across(c(3,5:11),
                                  as.factor)))
datlist_exp_food[-1]<-map(datlist_exp_food[-1],~.x |>
                             mutate(MONTH=as.Date(paste0("01-",MONTH),
                                                  format ="%d-%b %Y")))
datlist_exp_food$`2014-Jan`<-datlist_exp_food$`2014-Jan`|>
        mutate(MONTH=as.Date(paste0("01-",MONTH), format = "%d-%b-%y"))

share_food_lst<-map(datlist_exp_food,share_fn)

share_food_lst_region<-share_food_lst |>
                map(~.x |>
                            region_fn())
share_food_lst_region<-map(c(1,2,3),pluck_lst,lst=share_food_lst_region) |>
                set_names(c("Rural","Urban","Combined"))|>
                map(sort_lst_fn) |>
                map(list.rbind)

save(share_food_lst_region,file=here(clean_data,"food_share.RData"))
base::load(file=here(clean_data,"food_share.RData"))
## Food and Fuel

datlist_exp_food_fuel<-map(datlist,~.x %>%
                              map(labels_foodndfuel$Group,
                                  total_exp_fn,dat=.,vars_df=col_file$`Food&fuel`) |>
                              reduce(left_join)) |>
        map(~.x %>%
                    mutate("Total_Exp"=rowSums(dplyr::select(., starts_with("TOT_",ignore.case=F)),
                                               na.rm = TRUE))) |>
        map(~.x |>
                    mutate(across(c(3,5:11),
                                  as.factor)))
datlist_exp_food_fuel[-1]<-map(datlist_exp_food_fuel[-1],~.x |>
                                  mutate(MONTH=as.Date(paste0("01-",MONTH),
                                                       format ="%d-%b %Y")))
datlist_exp_food_fuel$`2014-Jan`<-datlist_exp_food_fuel$`2014-Jan`|>
        mutate(MONTH=as.Date(paste0("01-",MONTH), format = "%d-%b-%y"))

share_food_fuel_lst<-map(datlist_exp_food_fuel,share_fn)

share_food_fuel_lst_region<-share_food_fuel_lst |>
        map(~.x |>
                    region_fn())
share_food_fuel_lst_region<-map(c(1,2,3),pluck_lst,lst=share_food_fuel_lst_region) |>
        set_names(c("Rural","Urban","Combined"))|>
        map(sort_lst_fn) |>
        map(list.rbind)

# save(share_food_fuel_lst_region,file=here(clean_data,"food_fuel_share.RData"))
base::load(file=here(clean_data,"food_fuel_share.RData"))

#### Pi Ratio
base::load(file=here(clean_data,"pi_list_all_india.RData"))

#### Indirect Utility Calculation
alpha_food<-alpha_fn_state(pi_list$Combined,share_food_lst_region$Combined)
alpha_food<-alpha_food |> 
        select(c(1:5,38:41)) |> 
        mutate(V_tilda_hmt=Vhmt/Khmt)
# save(alpha_food,file=here(clean_data,"food_alpha.RData"))
