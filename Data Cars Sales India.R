library("readxl")
library("pacman")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

Data_raw <- import("Data/M5 Data Cars sales India.xlsx") %>%
    as_tibble 
Data_raw <- as.data.frame(Data_raw)
Data_raw

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
