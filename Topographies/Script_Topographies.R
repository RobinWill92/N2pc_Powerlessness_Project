library(remotes)
remotes::install_github("craddm/eegUtils")
library(eegUtils)
library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(cowplot)

# source("https://neuroconductor.org/neurocLite.R")
# neuro_install("PACKAGE")

chanlocs = read_table("Topographies/loc_csd.csd") %>% mutate(electrode = electrode %>% toupper())
chanlocs <- chanlocs[, c(1,4,2,3,5,6,7,8)] #lieber via select :)
# chanlocs[1, 1] = "Contra_C_Ipsi_C.Diff_all_NA"

#Block 1
eeg.map_Block1 = read_table("Topographies/Area_Block1_all_partic_all_Channel_Diff_all.txt", na="???") %>% 
  select(Vp, contains("all")) %>% select(-contains("Odd")) %>% select(-contains("Even")) %>% 
  summarise(across(-Vp, mean)) %>% #means across participants
  pivot_longer(cols=everything(), names_to="electrode", values_to="amplitude") %>% 
  separate(electrode, into=c("electrode", "condition"), sep="-") %>% mutate(condition = condition %>% gsub("Diff_all_", "", .)) %>% 
  mutate(electrode = electrode %>% gsub("Contra_", "", .) %>% gsub("_Ipsi_.+", "", .))
#eeg.map_Block1 %>% pull(electrode) %>% unique()
eeg.map_Block1 = eeg.map_Block1 %>% mutate(electrode = case_when(electrode == "C"  ~ "C3",
                                                                 electrode == "FT" ~ "FT9",
                                                                 electrode == "O"  ~ "O1",
                                                                 electrode == "T"  ~ "T7",
                                                                 electrode == "TP" ~ "TP9",
                                                                 electrode == "PO" ~ "PO7",
                                                                 T ~ electrode %>% substr(1, electrode %>% sapply(nchar) - 1))) %>% 
  mutate(electrodeChar = electrode %>% gsub("\\d", "", .), electrodeNum = electrode %>% gsub("\\D", "", .) %>% as.integer())
#eeg.map_Block1 %>% filter(electrode %in% chanlocs$electrode == F) #missing entries

#N2pc
eeg.map_Block1.m = eeg.map_Block1 %>% summarise(amplitude = mean(amplitude), .by=contains("electrode"))
eeg.map_Block1.m = eeg.map_Block1.m %>% mutate(electrodeNum = electrodeNum+1, electrode=paste0(electrodeChar, electrodeNum)) %>% 
  bind_rows(eeg.map_Block1.m) %>% arrange(electrode)
topo.n2pc_1 = eegUtils::topoplot(eeg.map_Block1.m, 
                                 #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                                 palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"

#Conditions separate (AN vs. NA)
eeg.map_Block1.AnNa = eeg.map_Block1 %>% 
  mutate(electrodeNum = electrodeNum + ifelse(condition=="NA", 0, 1), #NA left, i.e., angry processed left
         electrode = paste0(electrodeChar, electrodeNum))
topo.n2pc_1.AnNa = eegUtils::topoplot(eeg.map_Block1.AnNa, 
                                      #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                                      palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"


#Block 2
eeg.map_Block2 = read_table("Topographies/Area_Block2_all_partic_all_Channel_Diff_all.txt", na="???") %>% 
  select(Vp, contains("all")) %>% select(-contains("Odd")) %>% select(-contains("Even")) %>% 
  summarise(across(-Vp, mean)) %>% #means across participants
  pivot_longer(cols=everything(), names_to="electrode", values_to="amplitude") %>% 
  separate(electrode, into=c("electrode", "condition"), sep="-") %>% mutate(condition = condition %>% gsub("Diff_all_", "", .)) %>% 
  mutate(electrode = electrode %>% gsub("Contra_", "", .) %>% gsub("_Ipsi_.+", "", .))
#eeg.map_Block2 %>% pull(electrode) %>% unique()
eeg.map_Block2 = eeg.map_Block2 %>% mutate(electrode = case_when(electrode == "C"  ~ "C3",
                                                                 electrode == "FT" ~ "FT9",
                                                                 electrode == "O"  ~ "O1",
                                                                 electrode == "T"  ~ "T7",
                                                                 electrode == "TP" ~ "TP9",
                                                                 electrode == "PO" ~ "PO7",
                                                                 T ~ electrode %>% substr(1, electrode %>% sapply(nchar) - 1))) %>% 
  mutate(electrodeChar = electrode %>% gsub("\\d", "", .), electrodeNum = electrode %>% gsub("\\D", "", .) %>% as.integer())
#eeg.map_Block2 %>% filter(electrode %in% chanlocs$electrode == F) #missing entries

#N2pc
eeg.map_Block2.m = eeg.map_Block2 %>% summarise(amplitude = mean(amplitude), .by=contains("electrode"))
eeg.map_Block2.m = eeg.map_Block2.m %>% mutate(electrodeNum = electrodeNum+1, electrode=paste0(electrodeChar, electrodeNum)) %>% 
  bind_rows(eeg.map_Block2.m) %>% arrange(electrode)
topo.n2pc_2 = eegUtils::topoplot(eeg.map_Block2.m, 
                                 #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                                 palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"

#Conditions separate (AN vs. NA)
eeg.map_Block2.AnNa = eeg.map_Block2 %>% 
  mutate(electrodeNum = electrodeNum + ifelse(condition=="NA", 0, 1), #NA left, i.e., angry processed left
         electrode = paste0(electrodeChar, electrodeNum))
topo.n2pc_2.AnNa = eegUtils::topoplot(eeg.map_Block2.AnNa, 
                                      #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                                      palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"

####################################################
# Both Blocks

#N2pc
eeg.map_both = eeg.map_Block1.m %>% mutate(Block="1") %>% bind_rows(eeg.map_Block2.m %>% mutate(Block="2")) #block must be char for binding with "Average"
eeg.map_both = eeg.map_both %>% summarise(amplitude = mean(amplitude), .by = c("electrode", "electrodeChar", "electrodeNum")) %>% 
  mutate(Block="Average") %>% bind_rows(eeg.map_both) %>% mutate(Block = Block %>% as_factor())

topo.n2pc = eegUtils::topoplot(eeg.map_both %>% filter(Block=="Average"),
                               #eeg.map_both, groups="Block", #doesn't work :(
                               #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                               palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"


#Conditions separate (AN vs. NA)
eeg.map_both.AnNa = eeg.map_Block1.AnNa %>% mutate(Block="1") %>% bind_rows(eeg.map_Block2.AnNa %>% mutate(Block="2")) #block must be char for binding with "Average"
eeg.map_both.AnNa = eeg.map_both.AnNa %>% summarise(amplitude = mean(amplitude), .by = c("electrode", "electrodeChar", "electrodeNum")) %>% 
  mutate(Block="Average") %>% bind_rows(eeg.map_both.AnNa) %>% mutate(Block = Block %>% as_factor())


topo.n2pc.AnNa = eegUtils::topoplot(eeg.map_both.AnNa %>% filter(Block=="Average"),
                                    #eeg.map_both, groups="Block", #doesn't work :(
                                    #chanLocs = chanlocs, #there seems to be a default chanLocs file that recognizes electrode names in the 10-20 notation
                                    palette = "viridis", contour = FALSE, interp_limit = "head", highlights=c("P7", "P8")) #chan_marker="name"

#rows = Block (Average, 1, 2); cols = N2pc vs. Subcomponents (NA vs. AN)
#TODO unify ranges within N2pc plots and AnNa plots respectively for better comparison (i.e., within rows)
topo.all = cowplot::plot_grid(topo.n2pc, topo.n2pc_1, topo.n2pc_2,
                              topo.n2pc.AnNa, topo.n2pc_1.AnNa, topo.n2pc_2.AnNa,
                              nrow=2, labels="AUTO")
ggsave("plots/N2pc Topographies.png", plot=topo.all, dpi=300, type="cairo", width=16, height=7, units="cm", scale=2.5)
