---
title: "DataCleaning+DataExploration"
author: "AB"
date: "11/27/2018"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
df<-read.csv("bus-breakdown-and-delays 2.csv")
library(ggplot2)
library(dplyr)
library(caTools)
```
```{r}
#DATA CLEANING /FEATURE ENGINEERING/DIMENSION REDUCTION
df <- df[c(-2,-9,-14,-15,-16,-17,-18,-19)]

##dividing the data into two groups with how_long_delayed as NULL and one with NOT NULL
##Cant use how_long_delayed as NULL in test/train, but can be used as final test dataset

df.delay <- filter(df,How_Long_Delayed == "")
df <- filter(df,How_Long_Delayed !="")

#setting the Null values to NA
df[df == ""] = NA

# getting the sum of NA values
sum(is.na(df)) #5566

#converting the Boro column to character

df$Boro <- as.character(df$Boro)

#converting NA values to readable character FF
df$Boro[which(is.na(df$Boro))] <- "FF"

# Eliminate the NULL replaced columns 
df.temp <- filter(df,Boro == "FF")

#converting the route number to character and levelling them as per description of details about this dataset

df$Route_Number <- as.character(df$Route_Number)

df$Route.No.leveled <- nchar(df$Route_Number)
df$Route.No.leveled <- gsub("4","curb-to-curb",df$Route.No.leveled)
df$Route.No.leveled <- gsub("5","stop-to-school",df$Route.No.leveled)
df$Route.No.leveled <- gsub("1","Pre-K/EI Route",df$Route.No.leveled)
df$Route.No.leveled <- gsub("2","Pre-K/EI Route",df$Route.No.leveled)
df$Route.No.leveled <- gsub("3","Pre-K/EI Route",df$Route.No.leveled)

#Removing Route_Number column as its levelled
df <- df[,-4]

#filtering out schools with 0 and ` values

df <- filter(df,Schools_Serviced!="0")
df <- filter(df,Schools_Serviced!="`")

#taking the length of values for schools serviced column
df$Schools_Serviced <- as.character(df$Schools_Serviced)

df$Schools.Srvcd.length <- nchar(df$Schools_Serviced)

df$num.char <- substr(df$Schools_Serviced,1,1)

#levelling the schools serviced based on the length and number of alphabets in schools serviced column

df$Schools.Srvcd.lvld <- ifelse(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",df$num.char)==T ,"school-aged service","Pre-K/EI service")

df$Schools.Srvcd.lvld <- ifelse(df$Schools.Srvcd.length>5,"Multiple Sites",df$Schools.Srvcd.lvld)

df <- df[c(-5,-14,-15)]

# Getting the time component(with AM/PM) of bus breakdown occurence time

df$timecomponent <- substr(df$Occurred_On,12,13)

df$AMPMcomp <- substr(df$Occurred_On,21,22)

df$timecomponent <- as.numeric(df$timecomponent)

#creating a dummy variable for Boro

dummy_Boro<- model.matrix(~Boro -1, data=df)
df<- cbind(df[,-6],dummy_Boro)

#creating a dummy variable for Reason

df$Reason <- as.character(df$Reason)
df$Reason[is.na(df$Reason)] <- "OTHER"
df$Reason=as.factor(df$Reason)
dummy_Reason<- model.matrix(~Reason -1, data=df)
df<- cbind(df[,-4],dummy_Reason)

#Taking only the last part of bus companyname to easily dummify the data later
ptn <- "(.*? )"
df$abvtd_bcmpnme <- gsub(ptn, "", df$Bus_Company_Name)
df$abvtd_bcmpnme <- as.factor(df$abvtd_bcmpnme)

#Levelling the below mentioned column for linear regression
levels(df$School_Age_or_PreK) <- c(1,2)
levels(df$Breakdown_or_Running_Late) <- c(1,2)
df$Breakdown_or_Running_Late <- as.numeric(df$Breakdown_or_Running_Late)

# Removing factored columns
df <- df[c(-4,-5)]

# Levelling AM/PM component
df$AMPMcomp <- as.factor(df$AMPMcomp)
levels(df$AMPMcomp) <- c(1,2)

# Creating dummy variabe for route number levelled through model.matrix
dummy_Route.No.Lvld<- model.matrix(~Route.No.leveled -1, data=df)
df<- cbind(df[,-8],dummy_Route.No.Lvld)
#View(df)
# Creating dummy variabe for school service levelled through model.matrix
df$Schools.Srvcd.lvld <- as.factor(df$Schools.Srvcd.lvld)
dummy_Schools.Srvcd.Lvld<- model.matrix(~Schools.Srvcd.lvld -1, data=df)
df<- cbind(df[,-8],dummy_Schools.Srvcd.Lvld)

df$timecomponent <- as.numeric(df$timecomponent)

#creating dummy variable for school year
dummy_School.Year<- model.matrix(~School_Year -1, data=df)
df<- cbind(df[,-1],dummy_School.Year)

options(na.action='na.pass')
#View(df)
# Creating dummy variabe for Run_Type levelled through model.matrix
dummy_Run.Type<- model.matrix(~Run_Type -1, data=df)
df<- cbind(df[,-1],dummy_Run.Type)
df <- df[,-42]

#how long supposed to be in numeric, but as it as characters, removing characters to get pure numbers
#and to get good number of rows to test & train our dataset

df$How_Long_Delayed <- gsub("MINUTES","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("MIN","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("MINS","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("minutes","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("mins","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("min","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("Minutes","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("Min","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("S","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("mns","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("MN","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("mn","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("mnis","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("Mn","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("Mns.","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("10-15 MIIN","12.5",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("15 to 20m","17",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("m","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("MIIN","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("M","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("10-12","11",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("20-30","25",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("20-25","22",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("10-15","12",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("25-30","27",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("10-20","15",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("15-20","17",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("30-60","45",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("45-60","52",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1 hour","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1 HR","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1 hr","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("2HR","120",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1hr","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1 1/2HR","90",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1HOUR","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("1 Hour","60",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("i","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("I","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("u","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("U","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("t","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("T","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("e","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("E","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("s","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("S","",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("0.","0",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("0 .","0",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("5 .","5",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("2 .","2",df$How_Long_Delayed)
df$How_Long_Delayed <- gsub("5.","5",df$How_Long_Delayed)


df$How_Long_Delayed <- as.numeric(df$How_Long_Delayed)
df <- filter(df,How_Long_Delayed!="NA")
df <- filter(df,How_Long_Delayed <=48 & How_Long_Delayed > 0.55)

df$School_Age_or_PreK <- as.numeric(df$School_Age_or_PreK)
df$AMPMcomp <- as.numeric(df$AMPMcomp)

dummy_abvtd_bcmpnme<- model.matrix(~abvtd_bcmpnme -1, data=df)
df<- cbind(df[,-30],dummy_abvtd_bcmpnme)

df <- df[complete.cases(df[,]),]



library(dplyr)

df %>% select_if(~!is.numeric(.x)) %>% head()
df <- df[,sapply(df, is.numeric)]
df.corr <- cor(df)
df <- df[c(-5,-32,-33,-35,-36,-41,-44)]

library(caTools)
set.seed(101)
sample <- sample.split(df$How_Long_Delayed,SplitRatio = 0.7)
df.train <- subset(df,sample == TRUE,classProbs = TRUE)
df.test <- subset(df,sample == FALSE)
```

