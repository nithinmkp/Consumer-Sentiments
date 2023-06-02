
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions_3.R"))

## Packages ----
packages<-c("tidyverse","fs","janitor","here","foreign","haven","data.table","dtplyr")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
clean_data<-here("Data","Cleaned Data")
clean_master_data<-here(clean_data,"Master Data")
#--------------------------------------------------------------------------#

# Data ----
## Consumer Sentiments Data
base::load(here(clean_master_data,"datlist.RData"))
dat_CS<-dat_list |> 
        reduce(bind_rows) |> 
        dplyr::select(hh_id,date=month_slot,state,region_type,15:20,10:14)

## Macro Data
base::load(here(clean_master_data,"macro_data.RData"))
dat_macro<-datlist |> 
        map(~.x |> 
                    filter(date>=as.Date("2016-01-01") & date<=as.Date("2020-12-01"))) |> 
        reduce(left_join,by="date")
colnames(dat_macro)<-c("date","cpi","bond_rate","iip_change",
                       "pol_uncert","nse_change","bse_change",
                       "nse_sd","bse_sd","tb_3month")
dat_macro<-dat_macro |> 
        fill(tb_3month)

## People Data
base::load(here(clean_master_data,"people_data.RData"))

# ## Decomposition Data
# base::load(file = here(clean_data,"decomposed_data.RData"))
# 
# 
# decomp_dat_lst<-map(c(12,3,6,9),prime_Var_fn,dat=decomp_dat) |> 
#         map(~.x |> 
#                     drop_na()) |> 
#         set_names(paste0("lag_",c(12,3,6,9)))

## Indirect Utility ----
# base::load(file = here(clean_data,"indirect_util_states.RData"))
# indir_df<-state_indir_lst |> 
#         flatten() |> 
#         reduce(bind_rows)
# save(indir_df,file=here(clean_data,"indir_utility_comb.RData"))
# base::load(file=here(clean_data,"indir_utility_comb.RData"))


## Combined Data
indir_df<-indir_df |> 
        select(hh_id,date,state,region_type,Yhmt,Khmt,Phmt,Vhmt) |> 
        drop_na() 

indir_df<-calculate_lagged_values(indir_df,c("Yhmt", "Khmt", "Phmt", "Vhmt"),
                        c(12, 3, 6, 9))
indir_df<-indir_df[order(hh_id)]

jk<-growth_function(indir_df,c(12, 3, 6, 9),"Phmt","inf")


data_comb_lst<-map(decomp_dat_lst,left_join,dat_people,
               by=c("date","hh_id","state","region_type")) %>% 
        map(~.x %>% 
                    dplyr::filter(date>=as.Date("2016-01-01") & date<=as.Date("2020-12-01")) |> 
                    left_join(dat_CS,by=c("date","hh_id","state","region_type")) |> 
                    left_join(dat_macro,by=c("date")) |> 
                    drop_na() |> 
                    arrange(date))

save(data_comb,file = here(clean_master_data,"combined_data_no_lag.RData"))
save(data_comb_lst,file = here(clean_master_data,"combined_data_full.RData"))

# data_comb_stata_lst <-data_comb_lst|>
#         map(~.x |> 
#                     mutate(conditions_in_country_over_next_12_months=ifelse(conditions_in_country_over_next_12_months=="Better",1,0),
#                            across(c(3:4,14,16:32),as.factor)) %>% 
#                     mutate(stata_date=as.integer(format(as.yearmon(.$date),"%Y%m")),
#                            .before = state) |> 
#                     drop_na() |> 
#                     arrange(date) )
#          
# write.dta(data_comb_stata,file = here(clean_data,"reg_stata.dta"))
# iwalk(data_comb_stata_lst,~write.dta(.x,file = here(clean_data,paste0("reg_stata_data",
#                                                                      .y,".dta"))))
