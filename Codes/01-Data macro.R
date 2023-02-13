
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions.R"))

## Packages ----
packages<-c("tidyverse","fs","writexl","readxl","janitor","here","labelled","lubridate")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
clean_data<-here("Data","Cleaned Data")
dir_create(clean_data)
#--------------------------------------------------------------------------#

# Data ----

### Policy Uncertainty ----

dat<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "Sheet3") |> 
        mutate(date=make_date(year=Year,
                              month=Month,
                              day=1)) |> 
        rename_with(.cols = 3,~"PolicyUncertainty") |> 
        select(date,PolicyUncertainty) |> 
        slice(-n())


### IIP Index ----
#### Data
dat2<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "Sheet6",
                 range = "A3:M10") |> 
        rename_with(.cols = -1,~str_to_title(gsub("[()]","",.x))) |> 
        select(year=`Year / Month`,month.abb) |> 
        pivot_longer(cols = -"year",
                     names_to = "month") |> 
        mutate(month=match(month,month.abb),
               year_n=as.numeric(str_extract(year,"^[a-zA-Z0-9]{4}"))) |> 
        mutate(year_n=if_else(month %in% 1:3,year_n+1,year_n),
               date=make_date(year_n,month,day=1)) |> 
        select(date,IIP=value) |> 
        arrange(date)

#### Percentage change Data
dat2_change<-dat2 |> 
        mutate("per_change"=round(across(2,~(.x-lag(.x))/lag(.x)*100),3))

### T-Bill ----
dat3<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "Sheet7",
                 range = "A2:N23") |> 
        select(-...2,
               "Maturity/Year"="Terms To Maturity (In days)") |> 
        mutate(across(-1,as.numeric)) |> 
        filter(`Maturity/Year` %not_in% c("183-364")) |> 
        mutate(across(-1,~if_else(is.na(.x),lead(.x,1),.x))) |> 
        filter(`Maturity/Year` %not_in% c("92-182")) |> 
        select(year=`Maturity/Year`,month.abb) |> 
        pivot_longer(cols = -"year",
                     names_to = "month") |> 
        mutate(month=match(month,month.abb),
               year_n=as.numeric(str_extract(year,"^[a-zA-Z0-9]{4}"))) |> 
        mutate(year_n=if_else(month %in% 1:3,year_n+1,year_n),
               date=make_date(year_n,month,day=1)) |> 
        arrange(date) |> 
        select(date,rate=value)


### NSE Monthly ----
#### Data
dat4<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "Nifty-month",
                 range = "B4:N37") |> 
        select(year=`Year/ Month`,everything()) |> 
        mutate(across(-1,as.numeric)) |> 
        pivot_longer(-year,
                     names_to = "months",
                     values_to = "Nifty") |> 
        mutate(months=match(months,month.abb),
               year=str_extract(year,"^[0-9]{4}"),
               date=make_date(year,months,day=1)) |> 
        select(date,Nifty) |> 
        drop_na()
#### Percentage Change Data
dat4_change<-dat4 |> 
        mutate("per_change"=round(across(2,~(.x-lag(.x))/lag(.x)*100),3))

### BSE-50 Monthly ----
#### Data
dat5<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "bse50-month",
                 range = "B4:O37") |> 
        select(year=`Year/ Month`,everything()) |> 
        mutate(across(-1,as.numeric)) |> 
        pivot_longer(-year,
                     names_to = "months",
                     values_to = "BSE-50") |> 
        mutate(months=match(months,month.abb),
               year=str_extract(year,"^[0-9]{4}"),
               date=make_date(year,months,day=1)) |> 
        select(date,`BSE-50`) |> 
        drop_na()

#### Percentage Change Data
dat5_change<-dat5 |> 
        mutate("per_change"=round(across(2,~(.x-lag(.x))/lag(.x)*100),3))

### Bond rate ----
dat6<-read_excel(here(raw_data,"Nithin (1).XLSX"),sheet = "Sheet11",
                 range = "B5:Q253") |> 
        select(year=`Term to Maturity(Years)`,
               everything(),
               -c(...2,...6,...14)) |> 
        mutate(across(-1,as.numeric)) |> 
        filter(str_detect(year,"^([0-9]{4}|10)")) |> 
        mutate(across(-1,~if_else(is.na(.x),lead(.x,1),.x))) |> 
        filter(year %not_in% "10") |>
        mutate(year=as.numeric(str_extract(year,"^[0-9]{4}"))) |> 
        arrange(year) |> 
        pivot_longer(-year,
                     names_to = "months",
                     values_to = "10-Year-Bond") |> 
        mutate(months=match(str_to_title(months),month.abb),
               date=make_date(year,months,day=1)) |> 
        select(date,`10-Year-Bond`) |> 
        drop_na()


### Standard Deviation of Indices- Monthly SD of Daily Data ----

#### Daily Data 
##### NSE
dat7<-read_csv(here(raw_data,"^NSEI.csv"),
               col_select = c("Date","Close"))


##### BSE
dat8<-read_csv(here(raw_data,"CSVForDate.csv"),
               col_select = c("Date","Close"))


#### Standard Deviation Data 
##### NSE
dat7_sd<-dat7 |> 
        mutate(Close=as.numeric(Close)) |> 
        mutate(date=as.Date(Date,"%d-%m-%Y")) |> 
        select(date,Close)|> 
        mutate(year=year(date),
               month=month(date)) |> 
        group_by(year,month) |> 
        summarise(stdev=sd(Close,na.rm=T)) |> 
        mutate(date=make_date(year,month,day=1)) |>
        ungroup() |> 
        select(date,stdev)

##### BSE
dat8_sd<-dat8 |> 
        mutate(Close=as.numeric(Close)) |> 
        mutate(date=as.Date(Date,"%d-%B-%Y")) |> 
        select(date,Close)|> 
        mutate(year=year(date),
               month=month(date)) |> 
        group_by(year,month) |> 
        summarise(stdev=sd(Close,na.rm=T)) |> 
        mutate(date=make_date(year,month,day=1)) |>
        ungroup() |> 
        select(date,stdev)

### CPI Data ----

#### Data
dat9<-read_excel(here(raw_data,"HBS_Table_No._161___Consumer_Price_Index_-_Monthly.xlsx"),
                 range = "B21:N32") |>
        mutate(year=str_extract(`Year/Month`,"^[0-9]{4}")) |> 
        pivot_longer(-year,
                     names_to = "months",
                     values_to = "CPI") |> 
        mutate(year=as.numeric(year),
               months=str_to_title(months)) |> 
        filter(months !="Year/Month") |> 
        mutate(year=if_else(months %in% month.abb[1:3],
                            year+1,year),
               months=match(months,month.abb),
               date=make_date(year,months,day=1),
               CPI=as.numeric(CPI)) |> 
        select(date,CPI) |> 
        drop_na()

#### Percentage Change Data
dat9_change<-dat9 |> 
        mutate("per_change"=round(across(2,~(.x-lag(.x))/lag(.x)*100),3))

# Final Macroeconomic Data ----
datlist<-list(cpiausl=dat9_change,
              gs10=dat6,
              indpro=dat2_change,
              PolicyUncertainty=dat,
              sp500nseL=dat4_change,
              sp500bseL=dat5_change,
              sp500sdNSE=dat7_sd,
              sp500sdBSE=dat8_sd,
              tb3ms=dat3)
datlist[c(1,3,5,6)]<-map(datlist[c(1,3,5,6)], ~.x |> 
                                 select(-2))
