#setting the working directory

setwd("C:/Users/JACOPO/Desktop/Assignment 4")

#Install the required packages
install.packages("WDI")
install.packages("dplyr")
install.packages("plyr")
install.packages("XML")
#install.packages("remotes")
#remotes::install_github("NRGI/nrgiR")
install.packages("ggcorrplot")

library(WDI)
library(dplyr)
library(countrycode)
library(plyr)
library (tidyverse)
#library(remotes)
library(car) #new
library(corrplot) #new
library(ggcorrplot) #new

##### WDI #######
#Searching for required variable; Example:
WDIsearch(string = "gdp", field = "name", short = TRUE, cache = NULL)
WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp.*constant')
WDIsearch('*FINAL.ENERGY*')


#Download WDI dataset for required variables
FE = WDI(country = "all", indicator='1.1_TOTAL.FINAL.ENERGY.CONSUM', start=1990, end=2015)
#Final Energy Cons, Unit: TJ, terajoule = 10^6 MJ

GDP = WDI(country = "all", indicator='NY.GDP.MKTP.PP.KD', start=1990, end=2015)
# GDP-PPP, Unit: constant 2017 international $

#Check iso2c codes to merge the files
FE <- FE  %>% 
  mutate(iso3c = iso2c) #check iso codes   #dplyr

FE = subset(FE, select = -c(iso2c) )
GDP = subset(GDP, select = -c(iso2c) )

#Merging the datasets using inner join
WDI <- join_all(list(FE,GDP), type='left') #plyr, keep all the raws from the data frame on the left (FE)
WDI <- WDI %>% dplyr::rename_with(.cols = c(4:5), ~c("FE", "GDP"))

#Creating EI= FE/GDP
WDI <- WDI %>% 
  mutate(EI = FE/GDP) # [tJ/$] EI

summary(WDI)
summary(WDI$FE)

###### EPS ######

EPS <- read.csv("eps_2023.csv")

#Check out all variable names 
unique.default(EPS$Variable)

#Checking ISO2c codes of countries in EPS
countrycode(EPS$Country, origin = 'country.name', destination = 'iso3c') #countrycode

#Cleaning the dataset EPS
EPS <- EPS[EPS$VAR == 'EPS', ] #filtering EPS, this is the aggregate index, you can play with other Pol Ins
summary(EPS$Value)
EPS= subset(EPS, select = -c(2, 3,5, 7:12,14:15) ) #dropping variables
EPS <- EPS %>% dplyr::rename_with(.cols = c(1,3,4), ~c("iso3c","year", "EPS"))

#Merging EPS and WDI
dataset <- merge(WDI, EPS, by =c("iso3c","year")) #panel dataset with only rows from WDI and EPS

#Correlation between EPS and EI
Corr<- cor(dataset$EI, dataset$EPS, use = "complete.obs") #note that we are telling R to ignore the NAs
#corrplot(Corr, tl.col = "brown", tl.srt = 30, bg = "White",
#title = "\n\n Correlation Plot",
#type = "full")


library("ggplot2")

#By country
ggplot(data = dataset, aes(x = EPS, y = EI)) +
  geom_point() +
  facet_wrap(facets = vars(iso3c))

#ALL
ggplot(data = dataset, aes(x = EPS, y = EI)) +
  geom_point() 

group1 <- dataset %>%
  filter(iso3c %in% c("AUS", "CAN","CHN" , "CHE", "ESP",  "JPN", "NLD", "BRA", "MEX", "IDN", "IND","USA"))

ggplot(group1, aes(x = as.numeric(year))) +
  geom_line(aes(y = EPS, col = "lightblue"), show.legend = F) + 
  scale_y_continuous(
    # Features of the first axis
    name = "EPS Index",
    labels = scales::comma)+
  theme(
    axis.title.y = element_text(color = "lightblue", size=13),
    plot.title = element_text(size = 10, face = "bold")) +
  labs(x = "") +
  ggtitle("EPS from 1990 to 2015") +
  facet_wrap(facets = vars(iso3c))

ggplot(group1, aes(x = as.numeric(year))) +
  geom_line(aes(y = EI, col = "lightblue"), show.legend = F) + 
  scale_y_continuous(
    # Features of the first axis
    name = "EPS Index",
    labels = scales::comma)+
  theme(
    axis.title.y = element_text(color = "lightblue", size=13),
    plot.title = element_text(size = 10, face = "bold")) +
  labs(x = "") +
  ggtitle("EPS from 1990 to 2015") +
  facet_wrap(facets = vars(iso3c))



#### Fitting the model ######

## lm method and factor notation for fixed effects
LM0 <- lm(EI ~factor(country) +  EPS, data = group1)
LM1 <- lm(EI ~factor(country) + factor (year)+ EPS, data = group1)
summary(LM1)# check results
summary(LM0) 


#Change the unit of FE from TJ to EJ by multiplying by 1000000
group1 <- group1  %>%   mutate(EI2 = EI*1000000) 
# [tJ/$] EI
LM1 <- lm(EI2 ~factor(country) + factor (year)+ EPS, data = group1)
LM0 <- lm(EI2 ~factor(country) +  EPS, data = group1)
summary(LM0) 
summary(LM1)

#plotting Model 2
avPlots(LM1, main= paste("Model 1: Fixed effects using least squares dummy variable model"), col = carPalette()[5], col.lines = carPalette()[1])

#-------------------------------------------------

#Clearing the working environment before estimating the 2 models with CI as dependent variable
rm(list=ls(all=TRUE))

#Recalling the necessary libraries
library(WDI)
library(dplyr)
library(countrycode)
library(plyr)
library (tidyverse)
#library(remotes)
library(car) #new
library(corrplot) #new
library(ggcorrplot) #new

##### WDI #######
#Searching for required variable; Example:
WDIsearch(string = "gdp", field = "name", short = TRUE, cache = NULL)
WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp.*constant')
WDIsearch('*FINAL.ENERGY*')


#Download WDI dataset for required variables
FE = WDI(country = "all", indicator='1.1_TOTAL.FINAL.ENERGY.CONSUM', start=1990, end=2015)
#Final Energy Cons, Unit: TJ, terajoule = 10^6 MJ

GDP = WDI(country = "all", indicator='NY.GDP.MKTP.PP.KD', start=1990, end=2015)
# GDP-PPP, Unit: constant 2017 international $

CO2 = WDI(country = "all", indicator='EN.ATM.CO2E.KT', start=1990, end=2015)
#kiloton of CO2 = 1000 tCO2


#Check iso2c codes to merge the files
FE <- FE  %>% 
  mutate(iso3c = iso2c) #check iso codes   #dplyr

FE = subset(FE, select = -c(iso2c) )
GDP = subset(GDP, select = -c(iso2c) )
CO2 = subset(CO2, select = -c(iso2c) )

#Merging the datasets using inner join
WDI <- join_all(list(FE,GDP), type='left') #plyr, keep all the raws from the data frame on the left (FE)
WDI <- WDI %>% dplyr::rename_with(.cols = c(4:5), ~c("FE", "GDP"))

WDI <- join_all(list(CO2,GDP), type='left') #plyr, keep all the raws from the data frame on the left (FE)
WDI <- WDI %>% dplyr::rename_with(.cols = c(4:5), ~c("CO2", "GDP"))

#Creating CI= CO2/GDP
WDI <- WDI %>% 
  mutate(CI = CO2/GDP) 

summary(WDI)
summary(WDI$FE)

###### EPS ######

EPS <- read.csv("eps_2023.csv")

#Check out all variable names 
unique.default(EPS$Variable)

#Checking ISO2c codes of countries in EPS
countrycode(EPS$Country, origin = 'country.name', destination = 'iso3c') #countrycode

#Cleaning the dataset EPS
EPS <- EPS[EPS$VAR == 'EPS', ] #filtering EPS, this is the aggregate index, you can play with other Pol Ins
summary(EPS$Value)
EPS= subset(EPS, select = -c(2, 3,5, 7:12,14:15) ) #dropping variables
EPS <- EPS %>% dplyr::rename_with(.cols = c(1,3,4), ~c("iso3c","year", "EPS"))

#Merging EPS and WDI
dataset <- merge(WDI, EPS, by =c("iso3c","year")) #panel dataset with only rows from WDI and EPS

#Correlation between EPS and CI
Corr<- cor(dataset$CI, dataset$EPS, use = "complete.obs") #note that we are telling R to ignore the NAs
#corrplot(Corr, tl.col = "brown", tl.srt = 30, bg = "White",
#title = "\n\n Correlation Plot",
#type = "full")

#TRY TO DO SCATTERPLOT GGPLOT
#https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

library("ggplot2")

#By country
ggplot(data = dataset, aes(x = EPS, y = CI)) +
  geom_point() +
  facet_wrap(facets = vars(iso3c))

#ALL
ggplot(data = dataset, aes(x = EPS, y = CI)) +
  geom_point() 

group1 <- dataset %>%
  filter(iso3c %in% c("AUS", "CAN","CHN" , "CHE", "ESP",  "JPN", "NLD", "BRA", "MEX", "IDN", "IND","USA"))

ggplot(group1, aes(x = as.numeric(year))) +
  geom_line(aes(y = EPS, col = "lightblue"), show.legend = F) + 
  scale_y_continuous(
    # Features of the first axis
    name = "EPS Index",
    labels = scales::comma)+
  theme(
    axis.title.y = element_text(color = "lightblue", size=13),
    plot.title = element_text(size = 10, face = "bold")) +
  labs(x = "") +
  ggtitle("EPS from 1990 to 2015") +
  facet_wrap(facets = vars(iso3c))

ggplot(group1, aes(x = as.numeric(year))) +
  geom_line(aes(y = CI, col = "lightblue"), show.legend = F) + 
  scale_y_continuous(
    # Features of the first axis
    name = "EPS Index",
    labels = scales::comma)+
  theme(
    axis.title.y = element_text(color = "lightblue", size=13),
    plot.title = element_text(size = 10, face = "bold")) +
  labs(x = "") +
  ggtitle("EPS from 1990 to 2015") +
  facet_wrap(facets = vars(iso3c))

#### Fitting the model ######

## lm method and factor notation for fixed effects
LM0 <- lm(CI ~factor(country) +  EPS, data = group1)
LM1 <- lm(CI ~factor(country) + factor (year)+ EPS, data = group1)
summary(LM1)# check results
summary(LM0) 

#Change the unit of CI by multiplying by 1000000
group1 <- group1  %>%   mutate(CI2 = CI*1000000) 
# [tJ/$] EI
LM1 <- lm(CI2 ~factor(country) + factor (year)+ EPS, data = group1)
LM0 <- lm(CI2 ~factor(country) +  EPS, data = group1)
summary(LM0) 
summary(LM1)


#plotting Model 2
avPlots(LM1, main= paste("Model 1: Fixed effects using least squares dummy variable model"), col = carPalette()[5], col.lines = carPalette()[1])







