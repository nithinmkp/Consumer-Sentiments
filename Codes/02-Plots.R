
# Set-up ------------------------------------------------------------------
## Functions ----
source("Codes/functions.R")

## Packages ----
packages<-c("tidyverse","fs","ggthemes","showtext")
package_fn(packages)

## Folders and Files ----
raw_data<-"Data/Consumer Sentiments/Raw Data"
clean_data<-"Data/Consumer Sentiments/Cleaned Data"
clean_master_data<-paste0(clean_data,"/Master Data")

## Fonts ----
font_add_google("Oswald", "oswald")
font_add_google("Space Grotesk", "spacegrot")
#----------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------#
# ICS Trends plot ----
## Data ----
### Load Data ----
dat<-readRDS("Data/Consumer Sentiments/Cleaned Data/ICS.Rdata")

## Pivot Data ----

dat_pivot<-dat |> 
  pivot_longer(-month_slot,
               names_to = "Series",
               values_to = "Values") |>
  mutate(Series=as.factor(Series))
#----------------------------------------------------------------------------------#

# Plots ---------------------------------------------------------------------------
dat_list<-dat_pivot |> 
        group_split(Series) |> 
        set_names(levels(dat_pivot$Series))


plot_labels<-c(
        "Expected business condtions in next 12 months",
        "Expected business condtions in 5 years",
        "Expected Financial conditions in next 12 months",
        "Current Financial conditions compared with last 12 months",
        "Index of Consumer Sentiments",
        "Buying Conditions") 
names(plot_labels)<-levels(dat_pivot$Series)

dat_pivot |> 
        ggplot()+
        aes(x=month_slot,
            y=Values)+
        geom_line(aes(color=Series),
                  linewidth=1.3)+
        geom_point(aes(color=Series),
                       size=1.3)+
        facet_wrap(~Series,
                   ncol = 3,
                   scales = "free",
                   labeller = labeller(Series=plot_labels))+
        theme_wsj()+
        theme(
                legend.position = "none"
        )



tibble(df=dat_list,varname=names(dat_list),
       plt_title=plot_labels) %>% 
  mutate(plot=pmap(.,plot_fn,xvar=month_slot))  |> 
  mutate(filename = paste0("ggplots/",levels(dat_pivot$Series),".jpg"))  |>  
  select(plot,filename) %>% 
  pwalk(ggsave,width = 18,
        height = 12,
        units = "cm",
        dpi = 300) 

tibble(df=dat_list,varname=names(dat_list),
       plt_title=plot_labels)%>%
        pmap(plot_fn,xvar=month_slot)|> 
        patchwork::wrap_plots(ncol = 3) |> 
        ggsave(filename = paste0("ggplots/","fig2-repli",".jpg"),
               width = 32,
               height = 18.5,
               units = "cm",
               dpi = 300)

#--------------------------------------------------------------------------------#