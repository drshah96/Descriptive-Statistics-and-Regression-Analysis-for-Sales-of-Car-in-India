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

#Statistics Description of Efficiency
(Statistics_Efficiency_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4))),2)))
(Statistics_Efficiency_2 <- as.data.frame(round(stat.desc(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4))),2)))
(Statistics_Efficiency_3 <- as.data.frame(round(fivenum(as.numeric(substr(Data_raw$Efficiency,start = 1, stop = 4))),2)))

#Statistics Description of Power
(Statistics_Power_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2)))
(Statistics_Power_2 <- as.data.frame(na.omit(round(stat.desc(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2))))
(Statistics_Power_3 <- as.data.frame(na.omit(round(fivenum(as.numeric(substr(Data_raw$Power,start = 1, stop = 4))),2))))

#Statistics Description of Engine
(Statistics_Engine_1 <- as.data.frame(round(describe(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))
(Statistics_Engine_2 <- as.data.frame(round(stat.desc(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))
(Statistics_Engine_3 <- as.data.frame(round(fivenum(as.numeric(substr(Data_raw$Engine,start = 1, stop = 4))),2)))

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

### Shiny ###
install.packages("shiny")
library(shiny)

