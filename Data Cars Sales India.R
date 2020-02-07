library("readxl")
library("pacman")
library("dplyr")
library("ggplot2")
library("Rmisc")
library("DescTools")
library(plyr)
library(boot)
library(rcompanion)

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(plyr)){install.packages("plyr")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

Data_raw <- import("Data/M5 Data Cars sales India.xlsx") %>%
    as_tibble 
Data_raw <- as.data.frame(Data_raw)
Data_raw
