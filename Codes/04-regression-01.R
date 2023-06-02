
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
# base::load(file =  here(clean_master_data,"combined_data_no_lag.RData"))      
# data_comb[,28:32]<-data_comb[,28:32] |> 
#         map_df(~if_else(.x=="Better",1,0))
# data_comb[,c(3:4,9,11:27)]<-lapply(data_comb[,c(3:4,9,11:27)],as.factor)
# data_comb<-data_comb |>
#         mutate(across(c(Yhmt,Khmt,Phmt,Vhmt),log,.names = "log_{.col}"),
#                .before = 9) |>
#         group_by(hh_id) |>
#         arrange(hh_id) |>
#         mutate(across(c(Yhmt,Khmt,Phmt,Vhmt),
#                       dplyr::lag,n=1,.names = "lag_{.col}")) |> 
        # mutate(across(starts_with("log_"),dplyr::lag,n=1,
        #               .names = "lag_{.col}"))

# save(data_comb,file = here(clean_master_data,"reg_data_full.RData"))
base::load(file =   here(clean_master_data,"reg_data_full.RData"))
filter_states<-unique(data_comb$state)[-c(9,16,18,19:20,22)]
data_comb<-data_comb |> 
        filter(state %in% filter_states) |> 
        drop_na()

data_rural<-data_comb |> 
        filter(region_type=="RURAL")
data_urban<-data_comb |> 
        filter(region_type=="URBAN")

# # Convert your dataframe to a data.table
# dt <- as.data.table(data_comb)
# 
# # Set the key column for grouping
# setkey(dt, hh_id)
# 
# # Lag the specified columns within each group
# dt[, c("lag_12_Yhmt", "lag_12_Khmt", "lag_12_Phmt", "lag_12_Vhmt") := shift(.SD, n = 3), 
#    by = hh_id, .SDcols = c("Yhmt", "Khmt", "Phmt", "Vhmt")]
# 
# # Lag the columns starting with "log_" within each group
# dt[, paste0("lag_12_", grep("^log_", names(dt), value = TRUE)) := shift(.SD, n = 3),
#    by = hh_id, .SDcols = patterns("^log_")]

pdata_full<-pdata.frame(data_comb,index=c("hh_id","date"))

pdata_rural<-pdata.frame(data_rural,index=c("hh_id","date")) 
pdata_urban<-pdata.frame(data_urban,index=c("hh_id","date")) 


        
# pdata_reg_lst<-map(data_comb_lst,pdata.frame,index=c("hh_id","date"))


        
# pdata_reg_lst<-pdata_reg_lst |>
#         map(~.x |> 
#         mutate(conditions_in_country_over_next_12_months=ifelse(conditions_in_country_over_next_12_months=="Better",1,0),
#                across(c(3:4,14,16:38),as.factor)))



# Models ----
mod1<-conditions_in_country_over_next_12_months ~ log_Vhmt + region_type age_yrs + gender + 
        bond_rate  + nse_change + nse_sd  + tb_3month + state 
mod1_a<-conditions_in_country_over_next_12_months ~ log_Vhmt + age_yrs + gender + 
        bond_rate + iip_change   + nse_change + nse_sd  + tb_3month + pol_uncert + state 
mod2<-family_finances_a_year_later ~ log_Vhmt + age_yrs + region_type + gender + 
        bond_rate  + nse_change + nse_sd  + tb_3month + state 
mod2_a<-family_finances_a_year_later ~ log_Vhmt + age_yrs + gender + 
        bond_rate + iip_change   + nse_change + nse_sd  + tb_3month + pol_uncert + state 
mod2<-conditions_in_country_over_next_12_months ~ log_Yhmt + age_yrs + gender + 
        bond_rate + iip_change   + nse_change + nse_sd  + tb_3month + state 
mod3<-conditions_in_country_over_next_12_months ~ lag_log_Vhmt + age_yrs + region_type + gender + 
        bond_rate +  nse_change + nse_sd  + tb_3month + state 
mod3_a<-family_finances_a_year_later ~ lag_log_Vhmt + age_yrs + gender + 
        bond_rate +  nse_change + nse_sd  + tb_3month + state 
# mod3<-conditions_in_country_over_next_12_months ~ Yhmt + state + gender + region_type + age_yrs +
#         bond_rate + iip_change   + nse_change + nse_sd  + tb_3month
# mod4<-conditions_in_country_over_next_12_months ~ log_Yhmt + state + gender + region_type + age_yrs +
#         bond_rate + iip_change   + nse_change + nse_sd  + tb_3month
# mod5<-conditions_in_country_over_next_12_months ~ Phmt + state + gender + region_type + age_yrs +
#         bond_rate + iip_change   + nse_change + nse_sd  + tb_3month
# mod6<-conditions_in_country_over_next_12_months ~ log_Phmt + state + gender + region_type + age_yrs +
#         bond_rate + iip_change   + nse_change + nse_sd  + tb_3month


# Estimates ----
est_mod1_rural<-feglm(mod1,
                data = pdata_rural, 
                family = binomial("probit"))
est_mod1_urban<-feglm(mod1,
                data = pdata_urban, 
                family = binomial("probit"))
est_mod1_full<-feglm(mod1,
                      data = pdata_full, 
                      family = binomial("probit"))
est_mod1a_rural<-feglm(mod1_a,
                      data = pdata_rural, 
                      family = binomial("probit"))
est_mod1a_urban<-feglm(mod1_a,
                      data = pdata_urban, 
                      family = binomial("probit"))
est_mod2_rural<-feglm(mod2,
                      data = pdata_rural, 
                      family = binomial("probit"))
est_mod2_urban<-feglm(mod2,
                      data = pdata_urban, 
                      family = binomial("probit"))
est_mod2_full<-feglm(mod2,
                     data = pdata_full, 
                     family = binomial("probit"))
est_mod2a_rural<-feglm(mod2_a,
                       data = pdata_rural, 
                       family = binomial("probit"))
est_mod2a_urban<-feglm(mod2_a,
                       data = pdata_urban, 
                       family = binomial("probit"))

est_mod2_rural<-feglm(mod2,
                data = pdata_rural, 
                family = binomial("probit"))
est_mod2_urban<-feglm(mod2,
                     data = pdata_urban, 
                     family = binomial("probit"))
est_mod3_rural<-feglm(mod3,
                     data = pdata_rural, 
                     family = binomial("probit"))
est_mod3_urban<-feglm(mod3,
                     data = pdata_urban, 
                     family = binomial("probit"))
est_mod3_full<-feglm(mod3,
                     data = pdata_full, 
                     family = binomial("probit"))
est_mod3a_rural<-feglm(mod3_a,
                      data = pdata_rural, 
                      family = binomial("probit"))
est_mod3a_urban<-feglm(mod3_a,
                      data = pdata_urban, 
                      family = binomial("probit"))

est_mod_list<- mget(ls(pattern = "^est_mod"))
save(est_mod_list,file = here(models,"second.RData"))
base::load(file = here(models,"first.RData"))
# est_mod4<-pglm(mod4,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")
# est_mod4<-pglm(mod4,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")

# Marginals ----
mod1_marg<-probitmfx(mod1,
                     data = pdata_reg)
mod2_marg<-probitmfx(mod2,
                     data = pdata_reg)
mod3_marg<-probitmfx(mod3,
                     data = pdata_reg)
# mod4_marg<-probitmfx(mod4,
#                      data = pdata_reg)
# est_mod3<-pglm(mod3,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")
# est_mod4<-pglm(mod4,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")
# est_mod5<-pglm(mod5,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")
# est_mod6<-pglm(mod6,
#                data = pdata_reg, 
#                family = binomial("probit"),
#                effect = "individual")

# mod1 <- pglm(conditions_in_country_over_next_12_months ~ pi_dash + state + gender + region_type + age_yrs +
#                      cpi + bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#              data = pdata_reg, 
#              family = binomial("probit"),
#              effect = "individual")
# mod2<-pglm(conditions_in_country_over_next_12_months ~ x1 + state + gender + region_type + age_yrs +
#                     bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg, 
#            family = binomial("probit"),
#            effect = "individual")
# mod3<-pglm(conditions_in_country_over_next_12_months ~ x2 + state + gender + region_type + age_yrs +
#                     bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg, 
#            family = binomial("probit"),
#            effect = "individual")
# mod4<-pglm(conditions_in_country_over_next_12_months ~ high_util_hhs + state + gender + region_type + age_yrs +
#                    bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg, 
#            family = binomial("probit"),
#            effect = "individual")
# mod5<-pglm(conditions_in_country_over_next_12_months ~ x1+ high_util_hhs + state + gender + region_type + age_yrs +
#                    bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg, 
#            family = binomial("probit"),
#            effect = "individual")
# mod6<-pglm(conditions_in_country_over_next_12_months ~ growth_indir_util + state + gender + region_type + age_yrs +
#                    bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg, 
#            family = binomial("probit"),
#            effect = "individual")
# mod<-conditions_in_country_over_next_12_months ~ y_12_prime + state + gender + region_type + age_yrs +
#         bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month
# 
# mod7<-pglm(mod,
#            data = pdata_reg_lst$lag_12, 
#            family = binomial("probit"),
#            effect = "individual")
# mod8<-pglm(conditions_in_country_over_next_12_months ~ pi_12_prime + state + gender + region_type + age_yrs +
#                    bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg_lst$lag_12, 
#            family = binomial("probit"),
#            effect = "individual")
# mod9<-pglm(conditions_in_country_over_next_12_months ~ V_12 + state + gender + region_type + age_yrs +
#                    bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#            data = pdata_reg_lst$lag_12, 
#            family = binomial("probit"),
#            effect = "individual")

# mod10<-conditions_in_country_over_next_12_months ~ V + state + gender + region_type + age_yrs +
#         bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month
# mod11<-conditions_in_country_over_next_12_months ~ log_V + state + gender + region_type + age_yrs +
#         bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month
# # est_mod10<-pglm(mod10,
#                 data = pdata_reg, 
#                 family = binomial("probit"),
#                 effect = "individual")
# est_mod11<-pglm(mod11,
#                 data = pdata_reg, 
#                 family = binomial("probit"),
#                 effect = "individual")

# mod7_marg<-probitmfx(conditions_in_country_over_next_12_months ~ y1_prime + state + gender + region_type + age_yrs +
#                              bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#                      data = pdata_reg)
# mod8_marg<-probitmfx(conditions_in_country_over_next_12_months ~ pi_prime + state + gender + region_type + age_yrs +
#                              bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#                      data = pdata_reg)
# 
# mod9_marg<-probitmfx(conditions_in_country_over_next_12_months ~ V1 + state + gender + region_type + age_yrs +
#                   bond_rate + iip_change + pol_uncert + nse_change + nse_sd  + tb_3month,
#           data = pdata_reg) 
# 
# 
marg_mods<-mget(ls(pattern = "*_marg")) |>
        map(pluck("mfxest"))
