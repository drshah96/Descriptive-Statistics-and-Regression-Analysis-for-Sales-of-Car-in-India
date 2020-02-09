library("readxl")
library("pacman")
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(psych)
library(pastecs)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

Data_raw <- import("Data/M5 Data Cars sales India.xlsx")
Data_raw <- as.data.frame(Data_raw)
Data_raw

###### STATISTICS FOR POPULATION DATA (DESCRIPTIVE STATISTICS) #####

??stat.desc

#Statistics Description of Efficiency
#(Statistics_Efficiency_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4))),2)))
#(Statistics_Efficiency_2 <- as.data.frame(round(stat.desc(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4)),),2)))
#(Statistics_Efficiency_3 <- as.data.frame(round(fivenum(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4))),2)))

#Statistics Description of Power
#(Statistics_Power_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2)))
#(Statistics_Power_2 <- as.data.frame(na.omit(round(stat.desc(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2))))
#(Statistics_Power_3 <- as.data.frame(na.omit(round(fivenum(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2))))

#Statistics Description of Engine
#(Statistics_Engine_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))
#(Statistics_Engine_2 <- as.data.frame(round(stat.desc(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))
#(Statistics_Engine_3 <- as.data.frame(round(fivenum(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))

#Statistics Description of Price (USD) 
#(Statistics_Price_1 <- as.data.frame(round(describe(Data_raw$`In US Dollars (71.23 INR per USD)`),2)))
#(Statistics_Price_2 <- as.data.frame(round(stat.desc(Data_raw$`In US Dollars (71.23 INR per USD)`),2)))
#(Statistics_Price_3 <- as.data.frame(round(fivenum(Data_raw$`In US Dollars (71.23 INR per USD)`),2)))


#Combining Population Statistics Description
(Statistics_Description <- cbind(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4)),
                                 as.numeric(substr(Data_raw$Power,start = 1, stop = 4)), 
                                 as.numeric(substr(Data_raw$Engine,start = 1, stop = 4)),
                                 Data_raw$`In US Dollars (71.23 INR per USD)`))
options(scipen = 100)
options(digits = 2)
Statistics_Description <- as.data.frame(Statistics_Description)
names(Statistics_Description) <- c("Efficiency","Power","Engine","Price(USD)")
stat.desc(Statistics_Description, basic = F, p = 0.95)

Statistics_Description_1 <- as.data.frame(describe(Statistics_Description,skew = T))
row.names(Statistics_Description_1) <- c("Efficiency","Power","Engine","Price(USD)")
Statistics_Description_1
summary(Statistics_Description)

#Efficiency of Fuel according to Location and Fuel Type
Efficiency_Location_FuelType <- Data_raw %>%
    group_by(Location, `Fuel Type`) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Location_FuelType <- na.omit(Efficiency_Location_FuelType)
Efficiency_Location_FuelType

#Efficiency of Fuel according to Transmission in Year
Efficiency_Year_Transmission <- Data_raw %>%
    group_by(Year, Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Year_Transmission <- na.omit(Efficiency_Year_Transmission)
Efficiency_Year_Transmission

#Efficiency of Fuel according to Location, Fuel Type and Transmission
Efficiency_Location_FuelType_Transmission <- Data_raw %>%
    group_by(Location, `Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Location_FuelType_Transmission <- na.omit(Efficiency_Location_FuelType_Transmission)
Efficiency_Location_FuelType_Transmission

#Efficiency of Fuel according to Owner Type and Transmission
Efficiency_OwnerType_Transmission <- Data_raw %>%
    group_by(`Owner Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_OwnerType_Transmission <- na.omit(Efficiency_OwnerType_Transmission)
Efficiency_OwnerType_Transmission

#Efficiency of Fuel according to Fuel Type, Transmission and Seats
Efficiency_Seats_FuelType_Transmission <- Data_raw %>%
    group_by(Seats,`Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Seats_FuelType_Transmission <- na.omit(Efficiency_Seats_FuelType_Transmission)
Efficiency_Seats_FuelType_Transmission


##### INFERENTIAL STATISTICS #####

Data_Sample <- Data_raw[sample(nrow(Data_raw), 1500), ]
Data_Sample

#Statistics Description of Sample
(Statistics_Description_Sample <- cbind(as.numeric(substr(Data_Sample$Efficiency,start = 1, stop = 4)),
                                 as.numeric(substr(Data_Sample$Power,start = 1, stop = 4)), 
                                 as.numeric(substr(Data_Sample$Engine,start = 1, stop = 4)),
                                 Data_Sample$`In US Dollars (71.23 INR per USD)`))
options(scipen = 100)
options(digits = 2)
Statistics_Description_Sample <- as.data.frame(Statistics_Description_Sample)
names(Statistics_Description_Sample) <- c("Efficiency","Power","Engine","Price(USD)")
stat.desc(Statistics_Description_Sample, basic = F, p = 0.95)

Statistics_Description_Sample_1 <- as.data.frame(describe(Statistics_Description_Sample,skew = T))
row.names(Statistics_Description_Sample_1) <- c("Efficiency","Power","Engine","Price(USD)")
Statistics_Description_Sample_1
summary(Statistics_Description_Sample)

#Efficiency of Fuel according to Location and Fuel Type
Efficiency_Location_FuelType_Sample <- Data_Sample %>%
    group_by(Location, `Fuel Type`) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Location_FuelType_Sample <- na.omit(Efficiency_Location_FuelType_Sample)
Efficiency_Location_FuelType_Sample

#Efficiency of Fuel according to Transmission in Year
Efficiency_Year_Transmission_Sample <- Data_Sample %>%
    group_by(Year, Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Year_Transmission_Sample <- na.omit(Efficiency_Year_Transmission_Sample)
Efficiency_Year_Transmission_Sample

#Efficiency of Fuel according to Location, Fuel Type and Transmission
Efficiency_Location_FuelType_Transmission_Sample <- Data_Sample %>%
    group_by(Location, `Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Location_FuelType_Transmission_Sample <- na.omit(Efficiency_Location_FuelType_Transmission_Sample)
Efficiency_Location_FuelType_Transmission_Sample

#Efficiency of Fuel according to Owner Type and Transmission
Efficiency_OwnerType_Transmission_Sample <- Data_Sample %>%
    group_by(`Owner Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_OwnerType_Transmission_Sample <- na.omit(Efficiency_OwnerType_Transmission_Sample)
Efficiency_OwnerType_Transmission_Sample

#Efficiency of Fuel according to Fuel Type, Transmission and Seats
Efficiency_Seats_FuelType_Transmission_Sample <- Data_Sample %>%
    group_by(Seats,`Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_Seats_FuelType_Transmission_Sample <- na.omit(Efficiency_Seats_FuelType_Transmission_Sample)
Efficiency_Seats_FuelType_Transmission_Sample



### Shiny ###
install.packages("shiny")
library(shiny)

