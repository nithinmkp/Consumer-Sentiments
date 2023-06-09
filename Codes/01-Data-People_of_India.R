
# Set-up ------------------------------------------------------------------

## Functions ----
source(here::here("Codes","functions_3.R"))

## Packages ----
packages<-c("tidyverse","fs","janitor","here")
package_fn(packages)

## Folders and Files ----
raw_data<-here("Data","Raw Data")
people_path<-here(raw_data,"people_data")
clean_data<-here("Data","Cleaned Data")
clean_master_data<-here(clean_data,"Master Data")
#--------------------------------------------------------------------------#

# Data ----

char_cols<-c("HH_ID", "STATE", "REGION_TYPE", "MONTH_SLOT", "GENDER", "AGE_YRS","MEMBER_STATUS", 
             "RELATION_WITH_HOH", "RELIGION", "CASTE_CATEGORY", "EDUCATION", 
             "NATURE_OF_OCCUPATION", "HAS_BANK_AC", "HAS_CREDITCARD", "HAS_KISAN_CREDITCARD", 
             "HAS_DEMAT_AC", "HAS_PF_AC", "HAS_MOBILE")
data_lst<-list.files(people_path,"201[6-9]|2020",full.names = T) |> 
        map(read_data_fn,char_cols=char_cols)
dat_people<-data_lst |> 
        reduce(bind_rows) |>
        rename(date=month_slot)
save(dat_people,file=here(clean_master_data,"people_data.RData"))
