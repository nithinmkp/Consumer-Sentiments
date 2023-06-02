
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


transform_fn <- function(dt, cols, func, prefix, ...) {
        # Apply the function to selected columns with additional arguments
        new_cols <- lapply(dt[, ..cols], func, ...)
        
        # Generate column names with the specified prefix
        new_col_names <- paste0(prefix, cols)
        
        # Add the new columns to the data.table
        dt[, (new_col_names) := new_cols]
        
        # Return the modified data.table
        return(dt)
}



calculate_lagged_values <- function(data, columns, periods) {
        data <- as.data.table(data)
        
        # Calculate logarithm on selected columns
        log_columns <- paste0("log_", columns)
        data[, (log_columns) := lapply(.SD, log), .SDcols = columns]
        
        # Calculate lagged values for each period
        for (col in log_columns) {
                for (period in periods) {
                        lag_col_name <- paste0("lag_", period, "_", col)
                        data[, (lag_col_name) := shift(get(col), n = period), by = hh_id]
                }
        }
        
        return(data)
}


growth_function<-function(lagged_data, period_values, col, prefix) {
        pref_values <- vector("list", length(period_values))
        
        for (i in seq_along(period_values)) {
                period <- period_values[i]
                
                # Calculate inf values for the current period
                pref_column <- paste0(prefix, period)
                lag_column <- paste0("lag_", period, "_log_", col)
                
                # Create a new data frame with relevant columns
                pref_df <- copy(lagged_data)
                pref_df[[pref_column]] <- (log(lagged_data[[col]]) - lagged_data[[lag_column]])*100
                
                pref_df<-pref_df |> 
                        mutate(across(where(is.numeric),round,3))
                
                # Assign the name to the list element
                pref_values[[i]] <- pref_df
        }
        
        # Set names for the list elements
        pref_values <- setNames(pref_values, paste0(prefix, period_values))
        
        return(pref_values)
}





