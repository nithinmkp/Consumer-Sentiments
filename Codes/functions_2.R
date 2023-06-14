
# Functions ---------------------------------------------------------------

## Package Function ---- 
package_fn<-function(pkg){
        new.pkg<-setdiff(pkg,installed.packages()[,"Package"])
        if(length(new.pkg)){
                install.packages(new.pkg,dependencies = T)
                sapply(pkg,library,character.only=T)
        }else{
                sapply(pkg,library,character.only=T)
        }
}

## Not-in Function ----
`%not_in%`<-Negate("%in%")


# Functions for expenditure groups ----
read_fn<-function(filename,na_char,colum_select,n_lines=Inf){
        read_csv(filename,na=na_char,n_max = n_lines) %>% 
                filter(RESPONSE_STATUS=="Accepted") %>%
                select(intersect(colum_select,colnames(.)))
}

# Function for calculating shares
foodgroup_share_fn<-function(dat,spices_name,sweets_name,total_sum,share_grain,
                             share_spice,share_pulse,share_oil,share_fruits,
                             share_milk,share_sweets,share_meat){
        dat %>% 
                mutate({{spices_name}}:=monthly_expenon_dry_spices+
                               monthly_expennd_wet_spices+
                               monthly_expenes_and_onions,
                       {{sweets_name}}:=monthly_expennse_on_mithai+
                               monthly_expenense_on_bread+
                               monthly_expene_on_biscuits+
                               monthly_expen_salty_snacks+
                               monthly_expenes_and_flakes+
                               monthly_expenles_and_pasta+
                               monthly_expenake_ice_cream,
                       {{total_sum}}:=rowSums(.[ , c(3:18)])) |> 
                mutate({{share_grain}}:=monthly_expenls_wholegrain/get(total_sum),
                       {{share_spice}}:=get(spices_name)/get(total_sum),
                       {{share_pulse}}:=monthly_expenls_and_pulses/get(total_sum),
                       {{share_oil}}:=monthly_expenn_edible_oils/get(total_sum),
                       {{share_fruits}}:=monthly_expennse_on_fruits/get(total_sum),
                       {{share_milk}}:=monthly_expenmilk_prod/get(total_sum),
                       {{share_sweets}}:=get(sweets_name)/get(total_sum),
                       {{share_meat}}:=monthly_expeneggs_and_fish/get(total_sum))
        
}


# Functions for grouping households ----
lst_function<-function(lst){
        lst[[3]]<-list.rbind(lst)
        names(lst)[3]<-"Combined"
        lst
}

region_fn<-function(dat){
       dat_list<-dat |> 
                group_split(REGION_TYPE,.keep=T) |> 
                set_names(c("Rural","Urban")) |> 
                lst_function()
        
}

# some helper functions ----
pluck_lst<-function(lst,ind){
        lst |> 
                map(pluck(ind))
}

sort_lst_fn<-function(lst){
        lst[order(as.Date(paste0(names(lst),"-01"),format = "%Y-%b-%d"))]
}

# Total Expenditure Function ----

help_expen_fn<-function(dat,vars_df,selected_vars,subgroup_name,group_name,...){
      
        
        dat<-dat %>%
                dplyr::select(everything(),intersect(selected_vars,colnames(.))) 
        if("MONTHLY_EXPENSE_ON_MEAT_EGGS_AND_FISH" %in% colnames(dat)) {
                dat <- dat %>% 
                        dplyr::select(-MONTHLY_EXPENSE_ON_MEAT_EGGS_AND_FISH)
                
                dat %>% 
                        mutate("tot_{subgroup_name}":=rowSums(dplyr::select(., 
                                                                     intersect(selected_vars,colnames(.))), 
                                                              na.rm = TRUE))
        } else{
                dat %>% 
                        mutate("tot_{subgroup_name}":=rowSums(dplyr::select(., 
                                                                     intersect(selected_vars,colnames(.))), 
                                                              na.rm = TRUE))  
        }
        
}

total_exp_fn<-function(dat,vars_df,group_name,...){
        sub_groups<-vars_df |> 
                filter(Group=={{group_name}}) |> 
                pull(Subgroup) |> 
                unique()
        if(length(sub_groups)>1){
                selected_vars_list<-imap(sub_groups,~vars_df |> 
                                                 filter(Group=={{group_name}} & Subgroup=={{.x}}) |> 
                                                 pull(`CMIE Variables`)) |> 
                        set_names(sub_groups)
                names_subgrops<-names(selected_vars_list)
                map2(selected_vars_list,names_subgrops,help_expen_fn,dat=dat,
                     vars_df=vars_df,group_name=group_name) |> 
                        reduce(left_join) %>% 
                        mutate("TOT_{group_name}":=rowSums(dplyr::select(., starts_with("tot_",
                                                                                 ignore.case=F)), na.rm = TRUE))
        }else{
                
                selected_vars<-vars_df |> 
                        filter(Group=={{group_name}} & Subgroup=="Nil") |> 
                        pull(`CMIE Variables`)   
                dat %>%
                        select(everything(),intersect(selected_vars,colnames(.)))  %>% 
                        mutate("TOT_{group_name}":=rowSums(dplyr::select(., 
                                                                  intersect(selected_vars,colnames(.))), 
                                                           na.rm = TRUE)) 
        }
        
        
        
}

# CPI Matching Functions
help_cpi_fn<-function(dat,vars_df,selected_vars,subgroup_name,group_name,...){
        
        dat %>%
                select(everything(),intersect(selected_vars,colnames(.)))  %>% 
                mutate("{subgroup_name}":=rowSums(select(., 
                                                         intersect(selected_vars,colnames(.))), 
                                                  na.rm = TRUE)) 
        
}

cpi_match_fn<-function(dat,vars_df,group_name,...){
        sub_groups<-vars_df |> 
                filter(Maingroup=={{group_name}}) |> 
                pull(Subgroup) |> 
                unique()
        if({group_name}=="Food"){
                selected_vars_list<-imap(sub_groups,~vars_df |> 
                                                 filter(Maingroup=={{group_name}} & Subgroup=={{.x}}) |> 
                                                 pull(`cpisubcat`)) |> 
                        set_names(sub_groups)
                names_subgrops<-names(selected_vars_list)
                map2(selected_vars_list,names_subgrops,help_cpi_fn,dat=dat,
                     vars_df=vars_df,group_name=group_name) |> 
                        reduce(left_join) %>%
                        mutate("{group_name}":=rowSums(select(.,sub_groups),na.rm=T))
        }else if({group_name}=="Miscellaneous"){
                selected_vars<-vars_df |> 
                        filter(Maingroup=={{group_name}}) |> 
                        pull(`cpisubcat`)
                dat %>%
                        mutate("{group_name}":=rowSums(select(.,selected_vars),na.rm=T))
                
        }
        
        
        else{
                selected_vars<-vars_df |> 
                        filter(Maingroup=={{group_name}} & Subgroup=="Nil") |> 
                        pull(`cpisubcat`) 
                if(length(selected_vars)<=1){
                        dat
                }else{
                        dat %>%
                                mutate("{group_name}":=rowSums(select(.,selected_vars),na.rm=T))        
                }
        }
        
}

# Relative weights functions
weight_fn<-function(dat,vars_df,group_name,sub_groupname,select_varlst){
       
        dat |> 
                select({{select_varlst}},contains({{sub_groupname}}))
        
}

cpi_relative_fn<-function(dat,vars_df,group_name){
        sub_groups<-vars_df |> 
                filter(Maingroup=={{group_name}}) |> 
                pull(Subgroup) |> 
                unique()
        if({group_name}=="Food"){
                selected_vars_list<-imap(sub_groups,~vars_df |> 
                                                 filter(Maingroup=={{group_name}} & Subgroup=={{.x}}) |> 
                                                 pull(`cpisubcat`)) |> 
                        set_names(sub_groups)
                selected_vars_list
                names_subgrops<-names(selected_vars_list)
                
                map2(sub_groups,selected_vars_list,weight_fn,
                     dat=dat,
                     vars_df=vars_df,
                     group_name=group_name) |>
                        set_names(names_subgrops)
                
        }else if({group_name}=="Miscellaneous"){
                selected_vars<-vars_df |> 
                        filter(Maingroup=={{group_name}}) |> 
                        pull(cpisubcat)
                dat |> 
                        select(selected_vars,{group_name}) 
        }
        
        
        
        else{
                selected_vars<-vars_df |> 
                        filter(Maingroup=={{group_name}} & Subgroup=="Nil") |> 
                        pull(`cpisubcat`)  
                dat |> 
                        pull({{group_name}})
        }
}

# CPI Share calclulation
share_weights_fn<-function(lst){
        
        names_lst<-names(lst) 
        lst[[1]]<- map_at(lst[[which(names_lst==names_lst[1])]],c(2,6,8),
                          function(x){
                                  x |>
                                          mutate(across(-all_of(last_col()),
                                                        ~.x/across(last_col()),
                                                        .names = "share_{.col}")) |>
                                          select(starts_with("share"))
                          }
        )
        lst[length(lst)]<-map(lst[length(lst)],
                              function(x){
                                      x |>
                                              mutate(across(-all_of(last_col()),
                                                            ~.x/across(last_col()),
                                                            .names = "share_{.col}")) |>
                                              select(starts_with("share"))
                              })
        lst |>
                reduce(bind_cols) |>
                mutate(across(everything(),round,3)) |>
                rename_at(vars(12:18), ~ names_lst[2:8]) %>%
                mutate(across(where(~ any(is.na(.))), ~ ifelse(is.na(.), 0, .))) |> 
                select(where(~any(. >=0 & . < 1) )) |>
                rename_with(~ gsub("share_", "", .))
        
}

# CPI relative index function ----
mat_fn<-function(df,n_row=12){
        mat<-diag(0,nrow = n_row,ncol = ncol(df)*n_row)
        for(i in 1:n_row){
                mat[i,seq((i-1)*ncol(df),i*ncol(df))[-1]]<-unlist(df)
        }
        mat
}

cpi_re_index_fn<-function(df_cpi,df_share,df_match,group_name,dir_vars,...){
        if({group_name}=="Miscellaneous"){
                cat_vars<-df_match |> 
                        filter(Maingroup == {group_name} ) |> 
                        pull(cpisubcat)     
        }else{
                cat_vars<-df_match |> 
                        filter(Subgroup == {group_name} ) |> 
                        pull(cpisubcat)
        }
        df_share<-df_share %>%
                select(.,intersect(cat_vars,colnames(.)))
        df_cpi_re<-df_cpi |>
                filter(name %in% cat_vars) |>
                select(value) %>%
                mutate(value=replace_na(value,0)) 
        df_cpi_other<- df_cpi |> 
                filter(name %in% dir_vars) |> 
                pivot_wider(names_from = name,
                            values_from = value) |> 
                select(-c(1))
        
        
        df1<-mat_fn(df_share) %*% as.matrix(df_cpi_re) |>
                as_tibble() |>
                rename("{group_name}":=value) |> 
                mutate(across(everything(),round,2))
        bind_cols(df_cpi_other,df1)
        # df_share
        #df_cpi
        
        
}

## State Index Calculation
states_ind_fn<-function(state_lst){
        for(i in seq_along(state_lst)){
                state_lst[[i]]<-map(state_lst[[i]],~ .x %>%
                                            map(share_items, cpi_re_index_fn,
                                                df_cpi = .,
                                                df_share = cpi_share_lst[[i]],
                                                df_match = cpi_match_file,
                                                dir_vars=index_vars) |> 
                                            reduce(left_join))
        }
        return(state_lst)
}

# Expenditure Share Calculation Function
share_fn<-function(dat){
        dat |> 
                mutate(across(starts_with("TOT"),~round(.x/Total_Exp,3),
                              .names = "share_{.col}")) |> 
                select(-last_col()) |> 
                select(c(HH_ID,MONTH,REGION_TYPE,Total_Exp,STATE,DISTRICT,
                         starts_with("share"))) |> 
                 rename_at(-c(1:3),~ gsub("^share_tot_|^share_TOT_", "", .x)) 
}


## States_remame_function
states_rename_fn<-function(state,dat){
        dat$State=state
        dat
}


# Alpha Calculation
alpha_fn<-function(dat_cpi,dat_share){
        dat_share %<>%
                clean_names() %>% 
                select(hh_id,month,total_exp,intersect(colnames(dat_cpi)[-c(1)],
                                                       colnames(.)))
        dat_cpi |> 
                pivot_longer(-1) |> 
                select(date,pi=value,name) -> value_dict
        dat_share <-dat_share |> 
                rename(date=month) |> 
                pivot_longer(-c(1:3),
                             values_to = "share") |> 
                distinct_all() |> 
                left_join(value_dict,by=c("name","date")) |> 
                mutate(income_change=pi^share,
                       kh=share^share,
                       term2=pi^(-share)) |> 
                pivot_wider(names_from = name,
                            values_from = c(share,pi,income_change,kh,term2)) |> 
                mutate(across(-c(1:2),round,3))  
        dat_share$tot_change<-round(apply(dat_share[, grep("^income_change",
                                                           names(dat_share))], 1, prod),3)
        dat_share$term2_prod<-round(apply(dat_share[, grep("^term2",
                                                           names(dat_share))], 1, prod),3)
        dat_share$khmt<-round(apply(dat_share[, grep("^kh",
                                                     names(dat_share))], 1, prod),3)
        dat_share$indir_utility<-round(apply(dat_share[,c("khmt","term2_prod","total_exp")], 1, prod),3)
        dat_share
}

alpha_fn_state<-function(dat_cpi,dat_share){
        dat_share %<>%
                clean_names() %>% 
                select(hh_id,month,total_exp,state,region_type,intersect(colnames(dat_cpi)[-c(1)],
                                                             colnames(.)))
        dat_cpi |> 
                pivot_longer(-1) |> 
                select(date,pi=value,name) -> value_dict
        dat_share <-dat_share |> 
                rename(date=month) |> 
                pivot_longer(-c(1:5),
                             values_to = "share") |> 
                distinct_all() |> 
                left_join(value_dict,by=c("name","date")) |> 
                mutate(kh=share^share,
                       term2=pi^(share)) |> 
                pivot_wider(names_from = name,
                            values_from = c(share,pi,kh,term2)) |> 
                mutate(across(-c(1:5),round,3))  
        
        dat_share$Yhmt<-dat_share$total_exp
        dat_share$Phmt<-round(apply(dat_share[, grep("^term2",
                                                           names(dat_share))], 1, prod),3)
        dat_share$Khmt<-round(apply(dat_share[, grep("^kh",
                                                     names(dat_share))], 1, prod),3)

        dat_share<-dat_share |> 
                mutate(Vhmt=round((Khmt*Yhmt)/Phmt))
        dat_share
}

## Decomposition Function
decomp_fn<-function(dat){
        dat |> 
                group_by(hh_id) |> 
                arrange(hh_id) |>
                filter_all(all_vars(!is.nan(.))) |> 
                mutate(num_dates1 = n_distinct(date[date >= as.Date("2014-01-01") & 
                                                            date <= as.Date("2021-12-01")])) |> 
                filter(num_dates1 ==96) |> 
                mutate(pi_dash=(term2_prod/dplyr::lag(term2_prod,12))-1,
                       x1=(total_exp-dplyr::lag(total_exp,12))/dplyr::lag(total_exp,12)) |> 
                mutate(x2=pi_dash-x1) |> 
                mutate(growth_indir_util=(indir_utility/dplyr::lag(indir_utility,12))-1) |> 
                ungroup() |>
                group_by(hh_id) |> 
                arrange(hh_id) |> 
                mutate(num_dates = n_distinct(date[date >= as.Date("2015-01-01") & 
                                                           date <= as.Date("2020-12-01")])) |> 
                filter(num_dates ==72) |> 
                ungroup() |> 
                select(hh_id,date,state,region_type,tot_change,total_exp,indir_utility,
                       khmt,term2_prod,pi_dash,x1,x2,
                       growth_indir_util) |> 
                drop_na()
}
