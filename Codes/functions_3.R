
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


## type conversion function ---- 
convert_fn<-function(df, col_ind,fn,...) {
        df <- df %>% mutate(across(.cols = col_ind, .fns = fn,...))
}

## Rename Function ----
rename_fn <- function(df, col_ind, new_names) {
        names(df)[col_ind] <- new_names
        return(df)
}

## Read data function ----
read_data_fn<-function(filename,num_cols=NULL,char_cols){
        read_csv(file = filename,
                 col_select = c({{char_cols}})) |> 
                clean_names() |> 
                mutate(month_slot=as.Date(paste0("01-",month_slot),
                                          format = "%d-%b %Y")) 
}

prime_Var_fn<-function(dat,lag_len){
        dat |> 
                mutate("y_{lag_len}_prime":=(log(total_exp)-log(dplyr::lag(total_exp,lag_len))),
                       "pi_{lag_len}_prime":=(log(term2_prod)-log(dplyr::lag(term2_prod,lag_len))),
                       "kh_{lag_len}_prime":=(log(khmt)-log(dplyr::lag(khmt,lag_len)))) |> 
                mutate( !!paste0("V_", lag_len) := !!sym(paste0("kh_", lag_len, "_prime")) + 
                                !!sym(paste0("y_", lag_len, "_prime")) - 
                                !!sym(paste0("pi_", lag_len, "_prime")),
                        "log_V":=(log(indir_utility)),
                        V=indir_utility)
}

