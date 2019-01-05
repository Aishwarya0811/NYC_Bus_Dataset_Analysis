---
title: "FULL_RMD"
author: "Aish"
date: "11/5/2018"
output:
  html_document: default
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r NYbus}
library(ggplot2)
library(dplyr)
library(caTools)
library(MASS)
library(plyr)
df<-bus_breakdown_and_delays_2
```

```{r NYbus_1}
## School Bus Delay Places(Boro)
ggplot(data=df, aes(x = df$Boro)) +
  geom_bar(aes(fill=..count.., y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_fill_gradient("Count", low="green", high="red")+labs(title = "School Bus Delay Places", y = "Percentage", x = "Boro")
```
```{r NYbus_2}
## A breakdown or delay occurred on a specific category of busing service(Run_Type)
ggplot(data=df, aes(x = df$Run_Type)) +
  geom_bar(aes(fill= Run_Type, y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "A Breakdown or Delay Occurred on a Specific Category of Busing Service", y = "Percentage", x = "Timing")
```
```{r NYbus_3}
# the realtionship between year and Boro
df1 <- tbl_df(df)
class(df1)
df1

b_df1 <- filter(df1, School_Year == "2015-2016", Boro == "Staten Island")
b_df2 <- filter(df1, School_Year == "2015-2016", Boro == "Bronx")
b_df3 <- filter(df1, School_Year == "2015-2016", Boro == "Nassau County")
b_df4 <- filter(df1, School_Year == "2015-2016", Boro == "Brooklyn")
b_df5 <- filter(df1, School_Year == "2015-2016", Boro == "Connecticut")
b_df6 <- filter(df1, School_Year == "2015-2016", Boro == "Manhattan")
b_df7 <- filter(df1, School_Year == "2015-2016", Boro == "New Jersey")
b_df8 <- filter(df1, School_Year == "2015-2016", Boro == "Queens")
b_df9 <- filter(df1, School_Year == "2015-2016", Boro == "Rockland County")
b_df10 <- filter(df1, School_Year == "2015-2016", Boro == "Westchester")

count(b_df1)
count(b_df2)
count(b_df3)
count(b_df4)
count(b_df5)
count(b_df6)
count(b_df7)
count(b_df8)
count(b_df9)
count(b_df10)

# The 10 Places of School Buses Delayed during 2015-2016
slices <- c(2765, 17120, 835, 15639, 26, 11570, 358, 9874, 235, 2113) 
lbls <- c("Staten Island", "Bronx", "Nassau County", "Brooklyn", "Connecticut", "Manhattan", "New Jersey", "Queens", "Rockland County", "Westchester")
pct <- round(slices/sum(slices)*100, digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(10),
    main="The 10 Places of School Buses Delayed during 2015-2016")
```
```{r NYbus_4}
b1_df1 <- filter(df1, School_Year == "2016-2017", Boro == "Staten Island")
b1_df2 <- filter(df1, School_Year == "2016-2017", Boro == "Bronx")
b1_df3 <- filter(df1, School_Year == "2016-2017", Boro == "Nassau County")
b1_df4 <- filter(df1, School_Year == "2016-2017", Boro == "Brooklyn")
b1_df5 <- filter(df1, School_Year == "2016-2017", Boro == "Connecticut")
b1_df6 <- filter(df1, School_Year == "2016-2017", Boro == "Manhattan")
b1_df7 <- filter(df1, School_Year == "2016-2017", Boro == "New Jersey")
b1_df8 <- filter(df1, School_Year == "2016-2017", Boro == "Queens")
b1_df9 <- filter(df1, School_Year == "2016-2017", Boro == "Rockland County")
b1_df10 <- filter(df1, School_Year == "2016-2017", Boro == "Westchester")

count(b1_df1)
count(b1_df2)
count(b1_df3)
count(b1_df4)
count(b1_df5)
count(b1_df6)
count(b1_df7)
count(b1_df8)
count(b1_df9)
count(b1_df10)

# The 10 Places of School Buses Delayed during 2016-2017
slices_1 <- c(4114, 23389, 1103,  19637, 75, 16761, 526, 11050, 246, 2343) 
lbls <- c("Staten Island", "Bronx", "Nassau County",  "Brooklyn", "Connecticut", "Manhattan", "New Jersey", "Queens", "Rockland County", "Westchester")
pct <- round(slices_1/sum(slices_1)*100, digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices_1,labels = lbls, col=rainbow(10),
    main="The 10 Places of School Buses Delayed during 2016-2017")
```

```{r NYbus_5}
b2_df1 <- filter(df1, School_Year == "2017-2018", Boro == "Staten Island")
b2_df2 <- filter(df1, School_Year == "2017-2018", Boro == "Bronx")
b2_df3 <- filter(df1, School_Year == "2017-2018", Boro == "Nassau County")
b2_df4 <- filter(df1, School_Year == "2017-2018", Boro == "Brooklyn")
b2_df5 <- filter(df1, School_Year == "2017-2018", Boro == "Connecticut")
b2_df6 <- filter(df1, School_Year == "2017-2018", Boro == "Manhattan")
b2_df7 <- filter(df1, School_Year == "2017-2018", Boro == "New Jersey")
b2_df8 <- filter(df1, School_Year == "2017-2018", Boro == "Queens")
b2_df9 <- filter(df1, School_Year == "2017-2018", Boro == "Rockland County")
b2_df10 <- filter(df1, School_Year == "2017-2018", Boro == "Westchester")

count(b2_df1)
count(b2_df2)
count(b2_df3)
count(b2_df4)
count(b2_df5)
count(b2_df6)
count(b2_df7)
count(b2_df8)
count(b2_df9)
count(b2_df10)

# The 10 Places of School Buses Delayed during 2017-2018
slices_2 <- c(5994, 20282, 1343, 18367, 68, 23390, 521, 12294, 305, 1931) 
lbls <- c("Staten Island", "Bronx", "Nassau County",  "Brooklyn", "Connecticut", "Manhattan", "New Jersey", "Queens", "Rockland County", "Westchester")
pct <- round(slices_2/sum(slices_2)*100, digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices_2,labels = lbls, col=rainbow(10),
    main="The 10 Places of School Buses Delayed during 2017-2018")
```

```{r NYbus_6}
b3_df1 <- filter(df1, School_Year == "2018-2019", Boro == "Staten Island")
b3_df2 <- filter(df1, School_Year == "2018-2019", Boro == "Bronx")
b3_df3 <- filter(df1, School_Year == "2018-2019", Boro == "Nassau County")
b3_df4 <- filter(df1, School_Year == "2018-2019", Boro == "Brooklyn")
b3_df5 <- filter(df1, School_Year == "2018-2019", Boro == "Connecticut")
b3_df6 <- filter(df1, School_Year == "2018-2019", Boro == "Manhattan")
b3_df7 <- filter(df1, School_Year == "2018-2019", Boro == "New Jersey")
b3_df8 <- filter(df1, School_Year == "2018-2019", Boro == "Queens")
b3_df9 <- filter(df1, School_Year == "2018-2019", Boro == "Rockland County")
b3_df10 <- filter(df1, School_Year == "2018-2019", Boro == "Westchester")

count(b3_df1)
count(b3_df2)
count(b3_df3)
count(b3_df4)
count(b3_df5)
count(b3_df6)
count(b3_df7)
count(b3_df8)
count(b3_df9)
count(b3_df10)

# The 10 Places of School Buses Delayed during 2018-2019
slices_3 <- c(613, 4828, 336, 4073, 22, 7836, 86, 2465, 54, 441) 
lbls <- c("Staten Island", "Bronx", "Nassau County",  "Brooklyn", "Connecticut", "Manhattan", "New Jersey", "Queens", "Rockland County", "Westchester")
pct <- round(slices_3/sum(slices_3)*100, digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices_3,labels = lbls, col=rainbow(10),
    main="The 10 Places of School Buses Delayed during 2018-2018/10")
```

```{r NYbus_7}
# the realtionship between year and Reason
df3 <- df[-35580, ]
View(df3)
ggplot(data=df3, aes(x = School_Year)) +
  geom_bar(aes(fill=Reason, y = ..count..))+
  geom_text(aes(y = (..count..), label = scales::number(..count..)), stat = "count", vjust = -0.25) +
  labs(title = "The Reason of Delay during 2015-Present", x = "Year")
```

```{r NYbus_8}
# Breakdown_or_Running_Late
f_df11 <- filter(df1, Breakdown_or_Running_Late == "Breakdown")
f_df12 <- filter(df1, Breakdown_or_Running_Late == "Running Late")
count(f_df11)
count(f_df12)

slices_4 <- c(29103, 228613) 
lbls <- c("Breakdown", "Running Late")
pct <- round(slices_4/sum(slices_4)*100, digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices_4,labels = lbls, col=rainbow(10),
    main="Percentage of Delay Reasons by Bus Vendor")
```

```{r NYbus_9}
## the realtionship between year and delay mean
library(dplyr, warn.conflicts = F)

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
##
df$How_Long_Delayed <- as.numeric(df$How_Long_Delayed)
df6 <- filter(df,How_Long_Delayed!="NA")
df6 <- filter(df,How_Long_Delayed <=48 & How_Long_Delayed > 0.55)
```
```{r NYbus_9.1}
df_2 <- tbl_df(df6)
class(df_2)
df_2

bus <- group_by(df_2, School_Year)
bus

delaybus <- dplyr::summarise(bus, count = n(), delay = mean(How_Long_Delayed, na.rm = T))
delaybus

df.delaybus <- arrange(delaybus, desc(delay))
df.delaybusN <- df.delaybus[ ,-2]
df.delaybusN
```
```{r NYbus_9.2}
df.delaybus_1 <- round(df.delaybus$delay, digits = 2)

ggplot(data=df.delaybus, aes(x= df.delaybus$School_Year, y = df.delaybus$delay)) +
  geom_bar(fill = rainbow(4), stat="identity", position=position_dodge())+
  geom_text(aes(label= df.delaybus_1), vjust=-0.75, color="brown")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  labs(title = "The Relationship between Year and Average Delay Time", y = "Minutes", x = "Year")
```

```{r NYbus_10}
## the realtionship between Boro and delay mean
borogroup <- group_by(df_2, Boro)
borogroup

borogroupdelay <- dplyr::summarise(borogroup, count = n(), delay = mean(How_Long_Delayed, na.rm = T))
borogroupdelay
borogroupdelay1 <- borogroupdelay[-1:-2, ]

df.borogroupdelay <- arrange(borogroupdelay1, desc(delay))
df.borogroupdelayN <- df.borogroupdelay[ ,-2]
df.borogroupdelayN
```
```{r NYbus_10.1}
df.borogroupdelay_1 <- round(df.borogroupdelayN$delay, digits = 2)

ggplot(data=df.borogroupdelay, aes(x= df.borogroupdelay$Boro, y = df.borogroupdelay$delay)) +
  geom_bar(fill= rainbow(10), stat="identity", position=position_dodge())+
  geom_text(aes(label= df.borogroupdelay_1), vjust=-0.75, color="brown")+
  theme_minimal()+
  labs(title = "The Relationship between Boro and Average Delay Time", y = "Minutes", x = "Boro")
```

```{r NYbus_11}
## ## # the realtionship between Reason and delay mean
reasongroup <- group_by(df_2, Reason)
reasongroup

reasongroupdelay <- dplyr::summarise(reasongroup, count = n(), delay = mean(How_Long_Delayed, na.rm = T))
reasongroupdelay

df.reasongroupdelay <- arrange(reasongroupdelay, desc(delay)) 
df.reasongroupdelayN <- df.reasongroupdelay[ ,-2]
df.reasongroupdelayN
```
```{r NYbus_11.1}

df.reasongroupdelay_1 <- round(df.reasongroupdelay$delay, digits = 2)

ggplot(data=df.reasongroupdelay, aes(x= df.reasongroupdelay$Reason, y = df.reasongroupdelay$delay)) +
  geom_bar(fill=rainbow(10), stat="identity", position=position_dodge())+
  geom_text(aes(label= df.reasongroupdelay_1), vjust=-0.75, color="brown")+
  theme_minimal()+
  labs(title = "The Relationship between Reason and Average Delay Time", y = "Minutes", x = "Reason")
```

```{r}
df<-bus_breakdown_and_delays_2
str(df)

#dimension redution
df <- df[c(-2,-9,-14,-15,-16,-17,-18,-19)]
str(df)

##dividing the data into two groups with how_long_delayed as NULL and one with NOT NULL
##Cant use how_long_delayed as NULL in test/train, but can be used as final test dataset

df.delay <- filter(df, How_Long_Delayed == "")
df <- filter(df, How_Long_Delayed !="")

#setting the Null values to NA
df[df == ""] = NA

# getting the sum of NA values
sum(is.na(df))

#converting the Boro column to character

df$Boro <- as.character(df$Boro)

#converting NA values to readable character FF
df$Boro[which(is.na(df$Boro))] <- "FF"

# Eliminate the NULL replaced columns 
#making a temp dataframe with all the boro with FF
df.temp <- filter(df,Boro == "FF")
View(df.temp)
summary(df.temp)

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
# understood pattern recognition from the below mentioned site
#got code from http://www.jdatalab.com/data_science_and_data_mining/2017/03/20/regular-expression-R-part11.html for pattern used within grepl
df$Schools.Srvcd.lvld <- ifelse(grepl("[-]?[0-9]+[.]?[0-9]|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9][eE][0-9]+",df$num.char)==T ,"school-aged service","Pre-K/EI service")

df$Schools.Srvcd.lvld <- ifelse(df$Schools.Srvcd.length>5,"Multiple Sites",df$Schools.Srvcd.lvld)

df <- df[c(-5,-14,-15)]

# Getting the time component(with AM/PM) of bus breakdown occurence time

df$timecomponent <- substr(df$Occurred_On,12,13)
df$AMPMcomp <- substr(df$Occurred_On,21,22)
df$timecomponent <- as.numeric(df$timecomponent)

#creating a dummy variable for Boro using model.matrix and then dropping boro

dummy_Boro<- model.matrix(~Boro -1, data=df)
df<- cbind(df[,-6],dummy_Boro)

#creating a dummy variable for Reason

df$Reason <- as.character(df$Reason)
df$Reason[is.na(df$Reason)] <- "OTHER"
df$Reason=as.factor(df$Reason)
df$Reason
dummy_Reason<- model.matrix(~Reason -1, data=df)
df<- cbind(df[,-4],dummy_Reason)

#Taking only the last part of bus companyname to easily dummify the data later
#source from http://www.jdatalab.com/data_science_and_data_mining/2017/03/20/regular-expression-R-part11.html
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
df <- df[,-42] # removing 2018-2019

#how long should be numeric, but it is  character,therefore removing characters to get only numbers
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

skgendf <- data.frame(skgen=1:9924)
dfjoin <- data.frame(Bus_No=sort(unique(df$Bus_No)))
dfjoiner <- cbind(skgendf,dfjoin)
df <- merge(dfjoiner,df)
df <- df[,-1]
names(df)[names(df)=="skgen"] <- "Bus_No.sk"

library(dplyr)
df %>% select_if(~!is.numeric(.x)) %>% head()
df <- df[,sapply(df, is.numeric)]
df.corr <- cor(df)
df.corr
df <- df[c(-5,-32,-33,-35,-36,-41,-44)]
```

```{r NYbus_12.1}
library(caTools)
set.seed(101)
#splitting the dataset into test/train dataset 
sample <- sample.split(df$How_Long_Delayed,SplitRatio = 0.7)
df.train <- subset(df,sample == TRUE)
df.test <- subset(df,sample == FALSE)
View(df.test)
#Linear regression model application.
model.df <- lm(How_Long_Delayed ~., df.train)
library(MASS)
model.df.step <- stepAIC(model.df,direction="both")
class(model.df.step)
summary(model.df.step)
#summary(model.df)
```

```{r NYbus_12.2}
df.predict <- predict(model.df.step,df.test)
#View(df.predict)
results <- cbind(df.predict,df.test$How_Long_Delayed)
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
results
```

```{r NYbus_13}
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
### Generate Lift Chats
library(gains)
install.packages("gains")
df.predict <- predict(model.df.step,df.test)
View(df.predict)

gain <- gains(df.test$How_Long_Delayed, df.predict)

#Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(df.test$How_Long_Delayed))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(df.test$How_Long_Delayed))~c(0, dim(df.test)[1]), lty = 5)
```

```{r NYbus_14}
### Plot decile-wise chart
heights <- gain$mean.resp/mean(df.test$How_Long_Delayed)
decile_lift <- barplot(heights, names.arg = gain$depth,  ylim = c(0,3), col = "gold3",  
                       xlab = "Percentile", ylab = "Mean Response", 
                       main = "Decile-wise lift chart")
```

```{r NYbus_15}
#residual 
some.residuals <- results$real[1:500] - results$pred[1:500]

plot(some.residuals, type = "p", pch = 16,
     col = "blue1",
     ylab = "Sample Residuals", 
     ylim = c(-50, 50), bty = "n"
)
```
