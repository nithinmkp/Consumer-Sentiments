
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

## Table Function to word ----
table_fn <- function(dest, x, y) {
        addParagraph(dest, x, "\n")
        addTable(dest, y
        )
        addParagraph(dest, "\n")
}



## Data arrange (for adding dates) ----
data_arrange_fn<-function(x){
        ind<-index(x)
        x<-data.frame(x)
        rownames(x)<-NULL
        x  %>% 
                mutate(date=ind) %>% 
                select(date,dplyr::everything())
}

## Dataframe naming ----
df_rename_fn<-function(x,nam){
        x %>% data_arrange_fn() %>% 
                mutate(varname=nam) %>% 
                select(date,dplyr::everything())
}



## Not-in Function ----
`%not_in%`<-Negate("%in%")


## Data Importing ----
data_import_fn<-function(filename,num_cols,char_cols){
        read_csv(file = filename,
                 col_select = c(num_cols,{{char_cols}})) |> 
                clean_names() |> 
                mutate(month_slot=as.Date(paste0("01-",month_slot),
                                          format = "%d-%b %Y")) |> 
                mutate(across(10:14,~na_if(.,"Data Not Available"))) |> 
                drop_na()|> 
                mutate(conditions_in_country_over_next_12_months=case_when(
                        conditions_in_country_over_next_12_months=="Uncertain times"~"Same",
                        conditions_in_country_over_next_12_months=="Bad times"~"Worse",
                        conditions_in_country_over_next_12_months=="Good times"~"Better"
                ),
                conditions_in_country_over_next_5_years=case_when(
                        conditions_in_country_over_next_5_years=="Uncertain with ups and downs"~"Same",
                        conditions_in_country_over_next_5_years=="Continuously bad times"~"Worse",
                        conditions_in_country_over_next_5_years=="Continuously good times"~"Better"   
                ),
                is_this_good_time_to_buy_consumer_durables=case_when(
                        is_this_good_time_to_buy_consumer_durables=="Bad time"~"Worse",
                        is_this_good_time_to_buy_consumer_durables=="Good time"~"Better",
                        is_this_good_time_to_buy_consumer_durables=="Same as other times"~"Same"
                        
                ),
                across(10:14,as.factor))
}

## Category group
group_vars_fn<-function(dat,var_select,group_var){
        dat<- dat |> 
                pivot_longer(cols = var_select,
                             names_to = "Series",
                             values_to = "Values")
        name_series<-dat |> 
                select({{group_var}}) |> 
                distinct() |> 
                pull()
        dat |>
                group_by({{group_var}}) |>
                group_split(.keep=FALSE) |>
                set_names(name_series)
}

## Group Split ----
group_split_fn<-function(dat){
        name_vals<-unique(dat$Values)
        dat |> 
                group_split(Values,.keep = FALSE) |> 
                set_names(name_vals)
        
}

## Data transform ----
data_transform_fn<-function(dat,pivot_cols){
        dat<- dat |>  
                pivot_longer(cols = pivot_cols) |> 
                count(month_slot,name,value) |> 
                group_by(month_slot,name) |> 
                mutate(num=sum(n),
                       perc=(n/num)*100) |> 
                select(month_slot,name,value,perc) |> 
                group_by(name) 
        dat|> 
                group_split() |> 
                set_names(unique(dat$name))
}


## Index Calculation ----
ind_calculate_fn<-function(lst){
        lst |> 
                map(pivot_wider,
                    id_cols = month_slot,
                    names_from = value,
                    values_from = perc) |> 
                map(~.x |> 
                            mutate(
                                    ind=(`Better`-`Worse`)+100
                            )) |> 
                map_df(bind_cols,.id = "varname") |> 
                select(varname,month_slot,ind) |> 
                pivot_wider(id_cols = month_slot,
                            names_from = varname,
                            values_from = ind) |> 
                mutate(across(-month_slot,round,3))
        
}


## ICS Plot ----
showtext::showtext_auto()
plot_fn<-function(df,varname,xvar,plt_title,yscale=NULL,
                  plt_title.size=20,axix_title.size=14,
                  axis_text.size=14){
        df |> 
                ggplot()+
                aes(x={{xvar}},
                    y=Values)+
                geom_line(color="#588157")+
                geom_point(color="#fb5607")+
                theme_minimal()+
                theme(legend.position = "none",
                      panel.grid.minor = element_blank(),
                      axis.line = element_line(colour = "black"),
                      axis.text = element_text(family = "spacegrot",
                                               size = axis_text.size,
                                               colour = "#043572"),
                      plot.title = element_text(family = "oswald",
                                                colour = "#043572",
                                                size = plt_title.size),
                      axis.title = element_text(family = "spacegrot",
                                                size = axix_title.size,
                                                colour = "#043572")
                )+
                scale_x_date(breaks = "6 months",
                             date_labels = "%b-%y")+
                labs(title = plt_title,
                     x=NULL)
}
