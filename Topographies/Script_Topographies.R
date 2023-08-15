remotes::install_github("craddm/eegUtils")
library(remotes)
library(eegUtils)

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

# source("https://neuroconductor.org/neurocLite.R")
# neuro_install("PACKAGE")

setwd("G:/Data_PCs_ETH/Office PC/OB Sever/EEG Study_Dot Probe Task_RW_Final/EEG Study_Dot Probe Task_RW/Dot Probe Task 2_Data/GitHub/N2pc_Powerlessness_Project/Topographies")



chanlocs = read.delim("loc_csd.csd", , sep=" ", header = T)
chanlocs <- chanlocs[, c(1,4,2,3,5,6,7,8)]
# chanlocs[1, 1] = "Contra_C_Ipsi_C.Diff_all_NA"

eeg.map_Block1 = read.delim("Area_Block1_all_partic_all_Channel_Diff_all.txt", , sep=" ", header = T)
eeg.map_Block2 = read.delim("Area_Block2_all_partic_all_Channel_Diff_all.txt", , sep=" ", header = T)

# Average across all participants and excluding all columns except for _all_NA
eeg.map_Block1 = eeg.map_Block1 %>% summarise(across(everything(), mean)) %>% select(-contains("Odd")) %>% select(-contains("Even")) %>% 
  select(-contains("AN"))

eeg.map_Block1$Vp <- as.character(eeg.map_Block1$Vp)

eeg.map_Block1 = eeg.map_Block1 %>% replace_na(list(Vp = 'Average')) %>% 
  pivot_longer(Contra_C_Ipsi_C.Diff_all_NA:Contra_PO_Ipsi_PO.Diff_all_NA, names_to="electrode", values_to="amplitude") %>% 
  select(-c(X))

topoplot(eeg.map_Block1, palette = "inferno", 
         chanLocs = chanlocs, contour = FALSE,interp_limit = "head" )

  
  

