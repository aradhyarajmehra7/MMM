#############################################################################################################################################################
#                                                   :::::::: Market Mix Modelling ::::::::
#############################################################################################################################################################

### Business Understanding::

# An e-commerce firm specialising in electronic products. Over the last one year, they had spent a significant amount 
# of money in marketing. They also offered big-ticket promotions. 

# They are about to create a marketing budget for the next year which includes spending on commercials, online campaigns, 
# and pricing & promotion strategies.


### Objective::

# The aim is to develop a market mix model to observe the actual impact of different marketing variables over the last year.
# Basically needs to optimize the marketing levers to improve the revenue response.


#############################################################################################################################################################
#                                                     :::::::: Data Understanding ::::::::
#############################################################################################################################################################
getwd()
install.packages( "dplyr", dependencies = T ) ;
install.packages( "sqldf", dependencies = T ) ;
install.packages( "tidyr", dependencies = T ) ;
install.packages("readxl")
library(data.table)
options(java.parameters = "-Xmx1024m")
library(readxl)
setwd('C:/Users/aradh/Downloads/capstone')
#load library
install.packages('pacman')
pacman::p_load(pacman,dplyr,ggplot2,tidyr,readxl,ggvis,readr,tidyverse)
library( dplyr ) ;
library( sqldf ) ;
library( tidyr ) ;

### Data Load
eleckart <- read.csv("ConsumerElectronics.csv", stringsAsFactors = F)
media_investment <- read_excel("Media data and other information.xlsx", sheet = "Media Investment",skip=2)
nrow(eleckart)
ncol(eleckart)
str(eleckart)

### Data Preparation and cleaning and FE
eleckart_2015 <- eleckart %>%filter(eleckart$Year %in% c(2015) & eleckart$Month %in% c(7,8,9,10,11,12))
eleckart_2016 <- eleckart %>%filter(eleckart$Year %in% c(2016) & eleckart$Month %in% c(1,2,3,4,5,6))
eleckart<-rbind(eleckart_2015,eleckart_2016)
summary(eleckart)
eleckart<-eleckart%>% select(Year,Month,gmv,product_analytic_category,product_mrp,units)

### create a new column with no null values
eleckart<-eleckart%>% mutate(gmv_2= ifelse(gmv %in% c(NA),product_mrp-(product_mrp*0.15),
                                           ifelse(gmv==0,product_mrp-(product_mrp*0.15),gmv)))
summary(eleckart)

### drop all product with 0 Mrp
eleckart_updated<- eleckart%>%filter(eleckart$product_mrp!=0)
summary(eleckart_updated)
eleckart_updated<-eleckart_updated%>% select(Year,Month,product_mrp,units,gmv_2)

### pivot table
install.packages( "reshape2", dependencies = T ) ;
library( reshape2 ) ;
pivot <- eleckart_updated %>% 
  group_by( Year,Month ) %>% 
  summarise( Sales = sum( gmv_2 )) %>% 
  data.frame() ;


# create a csv file (Compiling data for model)
write.csv(pivot, "CE.csv", row.names = FALSE)
write.csv(media_investment,"MI.csv",row.names = FALSE)

### model creation
### read media investment (marketing) and SALES data
sales <- read.csv("CE.csv", stringsAsFactors = F)
marketing_data <- read.csv("MI.csv")
sales<- sales%>%select("Year","Month","Sales")

### marketing data has null values replace them with 0
marketing_data[is.na(marketing_data)] = 0
summary(marketing_data)

### join the data and keep it for model building
data <- left_join( marketing_data,
                   sales,by=c("Year","Month"))
data<-head(data,12)


data <- data%>%
  mutate(Sales2=Sales/10^7)
data<-data%>%select (-Year,-Month,-Total.Investment,-Sales)

write.csv(data,"data.csv",row.names = FALSE)
### multiple linear regression
linear_model<-lm(Sales2~.,data)
summary(linear_model)

### Contribution Charts
Base_Sale = linear_model$coefficients[1]
TV = linear_model$coefficients[2]*mean(data$TV)
Digital=linear_model$coefficients[3]*mean(data$Digital)
Sponsorship=linear_model$coefficients[4]*mean(data$Sponsorship)
Content.Marketing=linear_model$coefficients*mean(data$Content.Marketing)
Online.marketing=linear_model$coefficients[6]*mean(data$Online.marketing)
Affiliates=linear_model$coefficients[7]*mean(data$Affiliates)
SEM=linear_model$coefficients[8]*mean(data$SEM)
Radio = linear_model$coefficients[9]*mean(data$Radio)
Other = linear_model$coefficients[10]*mean(data$Other)


df_cc = data.frame(Medium = c("Base_Sale","TV","Digital","Sponsorship","Content.Marketing","Affiliates","SEM","Radio","Other"),Contribution = c(Base_Sale,TV,Digital,Sponsorship,Content.Marketing,Affiliates,SEM,Radio,Other))
df_cc$Percentage_Contribution = round(df_cc$Contribution*100/sum(df_cc$Contribution),2)
ggplot(df_cc,aes(y=Percentage_Contribution,x=Medium))+geom_bar(stat='identity',fill = 'darkred')+coord_flip()

### plot data
plot(data)

### total investment last year in marketing= budget last year= 846 cr
sum(as.numeric(media_investment$`Total Investment`), na.rm = TRUE)

### % of last year's budget
budget <- read_excel("Media data and other information.xlsx", sheet = "Sheet1")
percent <- function(x) {
  return (x*100)
}
  
budget$percent_Contri<-percent(budget$Contri)
summary(budget)

## this year we'll cut down the budget, first make the business profitable 
## we'll put money in following marketing space
#Digital, affiliates, radio, sponsorship, other

### New Marketing Budget= 509 Crore for next year 
    