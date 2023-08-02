if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
install.packages("janitor")
library(haven)
library(janitor)
library(dplyr)

se = function(x, na.rm = FALSE) { sd(x, na.rm) / sqrt(if(!na.rm) length(x) else length(na.omit(x))) }

# Mario template
path = "G:/Data_PCs_ETH/Office PC/OB Sever/EEG Study_Dot Probe Task_RW_Final/EEG Study_Dot Probe Task_RW/Dot Probe Task 2_Data/GitHub/N2pc_Powerlessness_Project"
path.ga_1 = "GA_P78_1_Diss_Raw Data_example.csv" %>% paste0(path, "/Grand Averages Robin_Mario/", .)

n2pc_1_diss_wide = read.csv2(path.ga_1) %>% 
  mutate(N2pc = contraP78 - ipsiP78) %>% select(c(1, 4:6)) 
names(n2pc_1_diss_wide) = c("Time", "ipsilateral", "contralateral", "N2pc")

n2pc_1_diss = n2pc_1_diss_wide %>% gather("ERP", "Voltage", 2:4)
n2pc_1_diss$ERP = factor(n2pc_1_diss$ERP, levels=c("contralateral", "ipsilateral", "N2pc")) #this order is important for the order in the legend
##

path = "G:/Data_PCs_ETH/Office PC/OB Sever/EEG Study_Dot Probe Task_RW_Final/EEG Study_Dot Probe Task_RW/Dot Probe Task 2_Data/GitHub/N2pc_Powerlessness_Project/Generic Exports Block 1 and 2/"
path.ga_1 = "N2pc_Block1_50Segm_only_compl_NA_AN_all_electrodes_modif.dat" %>% paste0(path,.)

n2pc_1_diss_wide = read.delim(path.ga_1, sep=" ", header = F) %>% t() %>% row_to_names(row_number = 1) %>% as.data.frame
rownames(n2pc_1_diss_wide) <-c(1:400)
vec <- c(-99:300) 
n2pc_1_diss_wide <- cbind(n2pc_1_diss_wide, Time = vec)
n2pc_1_diss_wide <- n2pc_1_diss_wide[,c(11,14,15)]

n2pc_1_diss = n2pc_1_diss_wide %>% gather("ERP", "Voltage", 1:2)
n2pc_1_diss$ERP <- as.factor(n2pc_1_diss$ERP) 
n2pc_1_diss$Voltage <- as.numeric(n2pc_1_diss$Voltage) 




# n2pc_1 plot -------------------------------------------------------------
n2pc_1_diss %>% ggplot(aes(x=Time, y=Voltage, color=ERP, group=ERP)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  scale_color_manual(values=c("red", "blue", "black")) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(path.ga_1 %>% gsub(".csv", ".png", .))