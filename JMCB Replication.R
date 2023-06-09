
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions_3.R"))

## Packages ----
packages<-c("tidyverse","fs","janitor","here","plm","pglm","marginaleffects",
            "modelsummary","emmeans","mfx","margins","data.table","fixest")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
clean_data<-here("Data","Cleaned Data")
clean_master_data<-here(clean_data,"Master Data")
models<-here("Models")
dir_create(models)
#----------------------------------------------------------------------------------#
# Data ----
## Consumer Sentiments Data 
base::load(here(clean_master_data,"datlist.RData"))
dat_CS<-dat_list |> 
        reduce(bind_rows) |> 
        dplyr::select(hh_id,date=month_slot,state,region_type,15:20,10:14)

## People Data
base::load(here(clean_master_data,"people_data.RData"))
dat_child<-dat_people |> 
        group_by(hh_id,date) |> 
        count(relation_with_hoh=="Son" | relation_with_hoh == "Daughter",
              name ="n_child" ) |> 
        rename(newcol=`relation_with_hoh == "Son" | relation_with_hoh == "Daughter"`) |> 
        filter(newcol==TRUE) |> 
        dplyr::select(-newcol)
dat_kid<-dat_people |> 
        group_by(hh_id,date) |> 
        filter(age_yrs<=12) |> 
        count(relation_with_hoh) |> 
        filter(relation_with_hoh %in% c("Grandchild","Son","Daughter")) |> 
        rename(n_kid=n) |> 
        dplyr::select(-3)
married_dat<-dat_people |> 
        group_by(hh_id,date) |> 
        filter(relation_with_hoh %not_in% c("Not Applicable","Data Not Available")) |> 
        mutate(marital_status=if_else("Spouse" %in% relation_with_hoh,
                                      "Yes","No")) |> 
        filter(relation_with_hoh=="HOH") |> 
        dplyr::select(hh_id,date,marital_status)

## Combined Data 
dat_comb1<-left_join(dat_CS,dat_child,
                     by=c("hh_id","date")) |> 
        left_join(dat_kid,by=c("hh_id","date")) |> 
        drop_na()
dat_comb2<-left_join(dat_people,married_dat,
                     by=c("hh_id","date"))
data_comb<-dat_comb1 |> 
        dplyr::select(hh_id,date,state,region_type,11:15,everything()) |> 
        left_join(dat_comb2,by=c("date","hh_id","state","region_type")) |> 
        drop_na() |> 
        mutate(across(-c(2:3),as.factor)) |> 
        mutate(across(5:9,\(x){
                case_when(x=="Better" ~1,
                x== "Worse"~ -1,
                x == "Same"~ 0)
        })) |> 
        filter(relation_with_hoh=="HOH")
filter_states<-c("Uttar Pradesh", "Maharashtra", "Kerala", "Madhya Pradesh", "Odisha", 
                 "Punjab", "Bihar", "Gujarat", "Tamil Nadu", "Rajasthan", "Delhi", "Karnataka", 
                 "Andhra Pradesh", "West Bengal", "Assam", "Telangana")
data_comb<-data_comb |> 
        filter(state %in% filter_states) |> 
        drop_na() |> 
        dplyr::filter(date>=as.Date("2016-01-01") & date<=as.Date("2020-02-01"))

data_comb<-data_comb |> 
        filter(nature_of_occupation %in% unique(data_comb$nature_of_occupation)[-c(4:5,7:11,14,19,21)])
rm(dat_child,dat_CS,dat_comb1,dat_comb2,married_dat,dat_list,dat_people,dat_kid)
#save(data_comb,file = here(clean_data,"jmcb_data.RData"))
#base::load(file = here(clean_data,"jmcb_data.RData"))
