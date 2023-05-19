
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
# 
# Data ----
# Column names
# col_file<-read_excel(here(raw_data,"cmie_vars_MASTER.xlsx"),sheet = "unmerged")
# col_names<-c("HH_ID","MONTH","STATE","HR","DISTRICT","AGE_GROUP","OCCUPATION_GROUP","EDUCATION_GROUP",
#              "GENDER_GROUP","SIZE_GROUP","REGION_TYPE",col_file$`CMIE Variables`)
# # 
# labels<-map(1:2,~col_file |>
#                     select(all_of(.x)) |>
#                     distinct() |>
#                     pull()) |>
#         set_names(colnames(col_file)[1:2])

## Load Data ----

### Expenditure Data ----
# datlist<-here(consumption_path,list.files(consumption_path,
#                                           pattern = "(consumption_pyramids_)*\\.csv")) |>
#         map(read_fn,na_char="-99",
#             colum_select=col_names) |>
#         set_names(format(ymd(paste0(gsub("(\\d{4})(\\d{2})", "\\1-\\2",gsub(".csv","",
#                        str_extract(list.files(consumption_path,
#                                               pattern = "(consumption_pyramids_)*\\.csv"),
#                                   "[0-9]{6}"))),"-01")),"%Y-%b"))

# save(datlist,file=here(clean_data,"consumption_full_data.RData")) # Save into Rdata format
# base::load(file=here(clean_data,"consumption_full_data.RData")) #load the already saved data

#### Calculate Total Expenditure for different groups ----

# datlist_exp<-map(datlist,~.x %>%
#                          map(labels$Group,
#                              total_exp_fn,dat=.,vars_df=col_file) |>
#                          reduce(left_join)) |>
#         map(~.x %>%
#                     mutate("Total_Exp"=rowSums(select(., starts_with("TOT_",ignore.case=F)),
#                                                na.rm = TRUE))) |> 
#         map(datlist_exp,~.x |> 
#                     mutate(across(c(3,5:11),
#                                   as.factor)))
# datlist_exp[-1]<-map(datlist_exp[-1],~.x |> 
#                              mutate(MONTH=as.Date(paste0("01-",MONTH),
#                                                   format ="%d-%b %Y")))
# datlist_exp$`2014-Jan`<-datlist_exp$`2014-Jan`|> 
#         mutate(MONTH=as.Date(paste0("01-",MONTH), format = "%d-%b-%y"))

# save(datlist_exp,file=here(clean_data,"consumption_tot_exp.RData"))


#### Calculate expenditure Shares ----
# load(file=here(clean_data,"consumption_tot_exp.RData")) #load the already saved data of expenditure totals

# share_data_list<-map(datlist_exp,share_fn)
# save(share_data_list,file=here(clean_data,"shares_data.RData"))
# base::load(here(clean_data,"shares_data.RData"))

### For all India
# share_data_cat_list<-share_data_list |>
#         map(~.x |>
#                     region_fn())
# 
# 
# share_data_cat_list<-map(c(1,2,3),pluck_lst,lst=share_data_cat_list) |>
#         set_names(c("Rural","Urban","Combined"))|>
#         map(sort_lst_fn) |>
#         map(list.rbind)
# save(share_data_cat_list,file=here(clean_data,"consumption_shares.RData"))

##### For States 
# state_names<-levels(share_data_list$`2022-Aug`$STATE)
# share_data_states_lst<-share_data_list |>
#         reduce(bind_rows) |>
#         mutate(STATE=factor(STATE,levels=state_names)) |>
#         group_split(STATE) %>%
#         set_names(state_names)
# 
# share_data_states_lst<-share_data_states_lst|>
#         keep(function(x) length(unique(x$REGION_TYPE)) > 1)
# 
# state_year_lst<-map(share_data_states_lst,~.x %>%
#                             distinct(year(MONTH))) %>%
#         map(~.x %>%
#                     pull(1) %>%
#                     nombre::cardinal())
# 
# share_data_states_lst<-share_data_states_lst %>%
#         map(~.x |>
#                     region_fn())
# names(share_data_states_lst)<-gsub("Jammu & Kashmir","Jammu and Kashmir",
#                                    names(share_data_states_lst))
# 
# save(share_data_states_lst,file=here(clean_data,"state_cons_shares.RData"))
# base::load(here(clean_data,"state_cons_shares.RData"))

#### CPI Weights ----
# file<-"Related Documents/CPI_weights.pdf"
# out<-extract_tables(file,page=3,output = "data.frame")
# 
# cpi_weights<-out[[1]] |>
#         select(-c(3,5:6,8:9,11)) |> 
#         drop_na() |> 
#         filter(str_detect(X.1, '\\d|Miscellaneous|') |X.1==" " ) |> 
#         mutate(IsNumber = grepl("^\\d+\\.\\d+\\.\\d+$", X.1),
#                Number = if_else(IsNumber, X.1, substr(X.1, 1, 6)),
#                Description = if_else(!IsNumber, substr(X.1, 8, nchar(X.1)), 
#                                      paste0(Number))) |> 
#         select(Description,num_range("X.",seq(3,7,by=2))) %>% 
#         rename_at(vars(starts_with("x.")),~ setNames(c("Rural","Urban","Combined"), 
#                                                      c("3", "5", "7"))[gsub("X\\.", "", .)]) |> 
#         slice(-c(1:8,n()-2,n()-1,n())) |>
#         mutate(across(-1,as.numeric)) |> 
#         filter(!is.na(Urban)) |>
#         mutate(Description=case_match(
#                 Description,
#                 "1.1.01"  ~ "Cereals and products",
#                 "1.1.08"  ~ "Pulses and products",
#                 "1.1.09"  ~ "Sugar and confectionery",
#                 "1.2.11"  ~ "Non-alcoholic beverages",
#                 "1.1.12"  ~ "Prepared meals, snacks, sweets etc.",
#                 "d light" ~ "Fuel and light",
#                 "6.1.01"  ~ "Household goods and services",
#                 "6.1.03"  ~ "Transport and communication",
#                 "6.1.04"  ~ "Recreation and amusement",
#                 "6.1.06"  ~ "Personal care and effects",
#                 .default = Description)
#         ) |> 
#         mutate(Description = case_match(row_number(),
#                                         13 ~"Food and beverages",
#                                         14 ~ "Pan, tobacco and intoxicants",
#                                         17 ~ "Clothing and footwear",
#                                         18 ~ "Housing",
#                                         .default = Description))
# 
# cpi_weights |> 
#         pivot_longer(-Description,
#                      names_to = "region",
#                      values_to = "index") |> 
#         mutate(region=factor(region,levels=c("Rural","Urban","Combined"))) |> 
#         group_split(region,.keep = F) |> 
#         map(~.x |>
#                     pivot_wider(names_from = Description,
#                                 values_from =index)
#         ) %>%
#         set_names(c("Rural","Urban","Combined"))->cpi_weights_region

#### CPI index matching ----
# cpi_match_file<-read_excel("Data/Raw Data/cmie_vars_MASTER.xlsx",
#                            sheet = "CPIweightedcat")
# 
# cpi_labels<-map(1:2,~cpi_match_file |>
#                         select(all_of(.x)) |>
#                         distinct() |>
#                         pull()) |>
#         set_names(colnames(cpi_match_file)[1:2])
# 
# cpi_matched_df_lst<-map(cpi_weights_region,
#                         ~.x %>%
#                                 map(cpi_labels$Maingroup,cpi_match_fn,
#                                     dat=.,
#                                     vars_df=cpi_match_file) |> 
#                                 reduce(left_join))

#### CPI Weights Recalculated

# cpi_re_weight_lst<-map(cpi_matched_df_lst,
#                        ~.x %>%
#                                map(cpi_labels$Maingroup,cpi_relative_fn,
#                                    dat=., vars_df=cpi_match_file) |> 
#                                set_names(cpi_labels$Maingroup))
#### CPI Shares ----
# cpi_share_lst<-map(cpi_re_weight_lst,share_weights_fn)


#### CPI Data ----
# cpi_all_india<-read_csv(here(raw_data,"CPI","CPIndex_Jan13-To-Dec22_data.csv"),
#                         skip=1,col_select = 1:9) |> 
#         mutate(date=make_date(Year,match(Month,month.name),"01"),
#                .before = State) |> 
#         select(-c(1:2,5:6))
# 
# cpi_data<-cpi_all_india |> 
#         pivot_longer(c(4:6),
#                      names_to = "Region",
#                      values_to = "CPI") |> 
#         mutate(Region=factor(Region,levels=c("Rural","Urban","Combined")),
#                Description=as.factor(Description)) |>
#         select(date,name=Description,Region,value=CPI) |> 
#         mutate(name=case_match(
#                 name,
#                 "Pan; tobacco; and intoxicants"       ~ "Pan, tobacco and intoxicants",
#                 "Prepared meals; snacks; sweets etc." ~ "Prepared meals, snacks, sweets etc.",
#                 .default = name 
#         ))  |>
#         group_split(Region,.keep = F) |> 
#         set_names(c("Rural","Urban","Combined")) |> 
#         map(~.x |> 
#                     group_split(year(date),.keep = F) |>
#                     map(~.x |>
#                                 arrange(date)) |> 
#                     set_names(nombre::cardinal(seq(2013,2022))))
# 
# 
#### CPI State level data ----
# cpi_states<-list.files(here(raw_data,"CPI"),"*[^data].csv", full.names = TRUE) |>
#         map(read_csv,skip=1,col_select = 1:9) |>
#         reduce(bind_rows) |>
#         mutate(date=make_date(Year,match(Month,month.name),"01"),.before = State)|>
#         select(-c(1:2,5:6)) ##original data
# 
# full_states<-c("Andhra Pradesh", "Assam", "Bihar", "Chhattisgarh", "Delhi", "Gujarat", "Haryana", 
#   "Himachal Pradesh", "Jammu and Kashmir", "Jharkhand", "Karnataka", "Kerala", 
#   "Madhya Pradesh", "Maharashtra", "Odisha", "Punjab", "Rajasthan", "Tamil Nadu", 
#   "Telangana", "Uttar Pradesh", "Uttarakhand", "West Bengal") ## identify states with full data

# cpistates_filterd<-cpi_states |> 
#         filter(State %in% full_states) ## filtered data
# 
# 
# 
# cpi_impute<-cpi_all_india |> 
#         filter(date >=as.Date("2020-03-01") & date <=as.Date("2020-07-01")) |> 
#         slice(-c(131,134:135))
# 
# cpi_impute<-map(full_states,states_rename_fn,dat=cpi_impute) |> 
#         reduce(bind_rows) |> 
#         mutate(Description=as.factor(Description),
#                State=as.factor(State)) |> 
#         filter(Description!="Consumer Food Price Index") ## data to impute for missing states data for period from march to july
# 
# 
# 
# 
# cpistates_filterd<-rows_append(cpistates_filterd,cpi_impute) |>
#         mutate(State=as.factor(State)) |> 
#         mutate(Description=case_match(
#                 Description,
#                 "Pan; tobacco; and intoxicants"       ~ "Pan, tobacco and intoxicants",
#                 "Prepared meals; snacks; sweets etc." ~ "Prepared meals, snacks, sweets etc.",
#                 .default = Description 
#         ))
# 
# cpi_states_lst<-cpistates_filterd |> 
#         group_split(State,.keep = T) |> 
#         map(~.x |> 
#                     pivot_longer(4:6,
#                                  names_to = "Region",
#                                  values_to = "CPI") |> 
#                     mutate(Region=factor(Region,levels=c("Rural","Urban","Combined")))|>
#                     group_split(Region,.keep = F) |> 
#                     set_names(c("Rural","Urban","Combined")) |> 
#                     map(~.x |> 
#                                 group_split(year(date),.keep = F) |> 
#                                 map(~.x |> 
#                                             arrange(date) |> 
#                                             select(date,
#                                                    name=Description,
#                                                    value=CPI)) |> 
#                                 set_names(nombre::cardinal(seq(2013,2022))))) |> 
#         set_names(levels(cpistates_filterd$State))


#### CPI Relative weights ----
# share_items<-c("Meat,fish and egg", "Vegetables and spices", "Sweets and snacks", 
#                "Miscellaneous")
# index_vars<-cpi_match_file |> 
#         filter(Maingroup %not_in% c("Miscellaneous")) |> 
#         filter(Subgroup %not_in% share_items ) |> 
#         pull(cpisubcat)
# cpi_re_index_lst<-list()
# for (i in seq_along(cpi_data)) {
#         cpi_re_index_lst[[i]] <- map(cpi_data[[i]], ~ .x %>%
#                                              map(share_items, cpi_re_index_fn,
#                                                  df_cpi = .,
#                                                  df_share = cpi_share_lst[[i]],
#                                                  df_match = cpi_match_file,
#                                                  dir_vars=index_vars) |> 
#                                              reduce(left_join))
# }
# cpi_re_index_lst<-cpi_re_index_lst |> 
#         set_names(c("Rural","Urban","Combined"))


#### States Relative Index
# state_lst_new<-map(cpi_states_lst, ~.x |> 
#                            map(~.x |> 
#                                        keep(~nrow(.x)>=324)))
# 
# states_lst_filter<-keep(state_lst_new, function(sublist) {
#         length(sublist) == 3 && all(map_int(sublist, length) == 9)
# })




# states_ind_lst<-list()
# 
# for(i in seq_along(states_lst_filter)){
#         states_ind_lst[i]<-map(states_lst_filter,states_ind_fn)   
# }
# 
# states_ind_lst<-states_ind_lst |> 
#         set_names(names(states_lst_filter))
# save(states_ind_lst,file = here(clean_data,"cpi_states.RData"))
# save(states_ind_lst,file = here(clean_data,"cpi_states_filtered.RData"))
#### Price Ratio Data ----
# pi_list<-map(cpi_re_index_lst,list.rbind) %>%
#         map(as.ts,start=2013,frequency=12) |>
#         map(as_tibble) |> 
#         map(~.x |> 
#                     mutate(across(where(is.numeric),~lead(.x,12)/.x,
#                                   .names = "pi_{.col}")) |> 
#                     select(starts_with("pi_")) |> 
#                     mutate(date=seq(as.Date("2013-01-01"),as.Date("2022-12-01"),by="month")) |> 
#                     drop_na() |>
#                     select(date,everything()) |> 
#                     rename_all(~gsub("^pi_", "", .)) |> 
#                     clean_names() |> 
#                     select(-`food_and_beverages`) |> 
#                     mutate(across(-1,round,3)))
# #save(pi_list,file=here(clean_data,"pi_list_all_india.RData"))
# 
# pi_list_states<-states_ind_lst |> 
#         map(~.x |> 
#                     map(list.rbind) |> 
#                     map(as_tibble) |> 
#                     map(~.x |> 
#                                 mutate(across(where(is.numeric),~lead(.x,12)/.x,
#                                               .names = "pi_{.col}")) |> 
#                                 select(starts_with("pi_")) |> 
#                                 mutate(date=seq(as.Date("2013-01-01"),
#                                                 as.Date("2021-12-01"),by="month")) |> 
#                                 drop_na() |>
#                                 select(date,everything()) |> 
#                                 rename_all(~gsub("^pi_", "", .)) |> 
#                                 clean_names() |> 
#                                 select(-`food_and_beverages`) |> 
#                                 mutate(across(-1,round,3))))
# save(pi_list_states,file = here(clean_data,"pi_list_states.RData"))
#save(pi_list_states,file = here(clean_data,"pi_list_states_filter.RData"))
#base::load(file = here(clean_data,"pi_list_states_filter.RData"))


#### Indirect Utility Calculation ----

# share_data_states_lst_filter<-share_data_states_lst[names(share_data_states_lst)%in% full_states]
# 
# share_data_states_lst_filter %<>%
#         map(~keep(.x=.,.p=names(.x)=="Combined"))
# pi_list_states %<>%
#         map(~keep(.x=.,.p=names(.x)=="Combined"))

# save(share_data_states_lst_filter,
# file = here(clean_data,"share_list_states_filter.RData"))
# base::load(file =  here(clean_data,"share_list_states_filter.RData"))
# state_indir_lst<-map2(pi_list_states,share_data_states_lst_filter,~map2(.x=..1,
#                                                                  .y=..2,
#                                                                  alpha_fn_state))


#save(state_indir_lst,file = here(clean_data,"indirect_util_states.RData"))

#### Decomposition Calculaation ----
#base::load(file = here(clean_data,"indirect_util_states.RData")) #load data
# decomp_dat<-map(state_indir_lst,~.x |>
#                         map(~.x |>
#                                     decomp_fn())) |>
#         flatten() |>
#         rlist::list.rbind()

# save(decomp_dat,file = here(clean_data,"decomposed_data.RData"))
