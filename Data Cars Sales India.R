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
names(Statistics_Description) <- c("Efficiency(Km/Kg or Km/L)","Power(bhp)","Engine(cc)","Price(USD)")
stat.desc(Statistics_Description, basic = F, p = 0.95)

Statistics_Description_1 <- as.data.frame(describe(Statistics_Description,skew = T))
row.names(Statistics_Description_1) <- c("Efficiency(Km/Kg or Km/L)","Power(bhp)","Engine(cc)","Price(USD)")
(Statistics_Description_1 <- data.frame(t(Statistics_Description_1)))
summary(Statistics_Description)

??group_by

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

###########################################################

Efficiency_OwnerType_Transmission_Fuel_Analysis <- Data_raw %>%
    group_by(`Owner Type`,`Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))
Efficiency_OwnerType_Transmission_Fuel_Analysis


shapiro.test(Efficiency_OwnerType_Transmission_Fuel_Analysis$Efficiency)

Price_OwnerType_Transmission_Fuel_Analysis <- Data_raw %>%
    group_by(`Owner Type`,`Fuel Type`,Transmission) %>%
    summarize(Price = mean(`In US Dollars (71.23 INR per USD)`))

Price_OwnerType_Transmission_Fuel_Analysis$Parameter <- paste0(Price_OwnerType_Transmission_Fuel_Analysis$`Owner Type`," ",
                                                             Price_OwnerType_Transmission_Fuel_Analysis$`Fuel Type`," ",
                                                             Price_OwnerType_Transmission_Fuel_Analysis$Transmission)
Price_OwnerType_Transmission_Fuel_Analysis

shapiro.test(Price_OwnerType_Transmission_Fuel_Analysis$Price)


Analysis <- data.frame(Price_OwnerType_Transmission_Fuel_Analysis$Parameter,
                       Efficiency_OwnerType_Transmission_Fuel_Analysis$Efficiency,
                       Price_OwnerType_Transmission_Fuel_Analysis$Price)

names(Analysis) <- c("Parameter", "Efficiency", "Price")
Analysis$Efficiency <- round(Analysis$Efficiency,2)
Analysis$Price <- round(Analysis$Price,2)
Analysis

################################################


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
(Statistics_Description_Sample_1 <- data.frame(t(Statistics_Description_Sample_1)))
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

# Price of Car according to OwnerType, Transmission
Price_OwnerType_Transmission_Sample <- Data_Sample %>%
    group_by(`Owner Type`,Transmission) %>%
    summarize(Price = mean(`In US Dollars (71.23 INR per USD)`))
Price_OwnerType_Transmission_Sample

#Price of car according to Location, OwnerType and Transmission
Price_Location_OwnerType_Transmission_Sample <- Data_Sample %>%
    group_by(Location,`Owner Type`,Transmission) %>%
    summarize(Price = mean(`In US Dollars (71.23 INR per USD)`))
Price_Location_OwnerType_Transmission_Sample

# Price of Car according to OwnerType, Transmission and Fuel
Price_OwnerType_Transmission_Fuel_Sample <- Data_Sample %>%
    group_by(`Owner Type`,`Fuel Type`,Transmission) %>%
    summarize(Price = mean(`In US Dollars (71.23 INR per USD)`))

shapiro.test(Price_OwnerType_Transmission_Fuel_Sample$Price)
Price_OwnerType_Transmission_Fuel_Sample$Parameter <- paste0(Price_OwnerType_Transmission_Fuel_Sample$`Owner Type`," ",
                                                             Price_OwnerType_Transmission_Fuel_Sample$`Fuel Type`," ",
                                                             Price_OwnerType_Transmission_Fuel_Sample$Transmission)
Price_OwnerType_Transmission_Fuel_Sample

#Efficiency of Fuel according to Owner Type,Fuel Type and Transmission
Efficiency_OwnerType_Transmission_Fuel_Sample <- Data_Sample %>%
    group_by(`Owner Type`,`Fuel Type`,Transmission) %>%
    summarize(Efficiency = mean(as.numeric(substr(Efficiency,start = 1, stop = 4))))

shapiro.test(Efficiency_OwnerType_Transmission_Fuel_Sample$Efficiency)

Efficiency_OwnerType_Transmission_Fuel_Sample$Parameter <- paste(Efficiency_OwnerType_Transmission_Fuel_Sample$`Owner Type`," ",
                                                                  Efficiency_OwnerType_Transmission_Fuel_Sample$`Fuel Type`," ",
                                                                  Efficiency_OwnerType_Transmission_Fuel_Sample$Transmission)
Efficiency_OwnerType_Transmission_Fuel_Sample

df <- Data_Sample %>%
    group_by(`Owner Type`,`Fuel Type`,Transmission) %>%
    summarize(Efficiency = (mean(as.numeric(substr(Efficiency,start = 1,stop = 4)))))
df
Scatterplot_Efficiency_Price <- data.frame(Price_OwnerType_Transmission_Fuel_Sample$Parameter,
                                           df$Efficiency,
                                           Price_OwnerType_Transmission_Fuel_Sample$Price) %>%
    as_tibble()

names(Scatterplot_Efficiency_Price) <- c("Parameters","Efficiency(KM/KG or KM/L)","Price(USD)")
Scatterplot_Efficiency_Price$Efficiency <- round(Scatterplot_Efficiency_Price$Efficiency,2)
Scatterplot_Efficiency_Price$Price <- round(Scatterplot_Efficiency_Price$Price,2)
Scatterplot_Efficiency_Price

##### Graphs #####

#Correlation Graph 1
library("ggpubr")
ggscatter(Scatterplot_Efficiency_Price, 
          x = "Efficiency",
          y = "Price",
          add = "reg.line", conf.int = TRUE, 
          #color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          cor.coeff.args = list(method = "pearson", label.sep = "\n"),
          #add = "loess",
          ellipse = TRUE, mean.point = TRUE,
          show.legend.text = TRUE,
          star.plot = TRUE, 
          #panel.labs = Scatterplot_Efficiency_Price$Parameters,
          #title = "Fig 1: Correlation between Price and Efficiency of Car based on OwnerType, FuelType and Transmission",
          #ggtheme = theme_dark(),
          xlab = "Efficiency (Km/Kg or Km/L)", ylab = "Price(USD)", repel = TRUE) 

# Correalation Graph 2
ggplot(Scatterplot_Efficiency_Price, aes(x = Scatterplot_Efficiency_Price$Efficiency,
                                         y = Scatterplot_Efficiency_Price$Price)) + 
    geom_point() + 
    #geom_text(label=Scatterplot_Efficiency_Price$Parameters, 
    #          hjust=1.5, vjust = 0,
    #          check_overlap = TRUE) + 
    ggtitle("International vs Domestic States MigrationRate") +
    xlab("International Migration Rate") + ylab("Domestic Migration Rate") +
    geom_smooth(method = lm)



##### Formatting Tables #####
install.packages("formattable")
library(knitr)
library(formattable)

df <- formattable(Price_OwnerType_Transmission_Sample, list(
    price = formatter("span", 
                          style = ~ style(color = ifelse(Transmission = 'Automatic', "green", "red")))))
df
