library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

se = function(x, na.rm = FALSE) { sd(x, na.rm) / sqrt(if(!na.rm) length(x) else length(na.omit(x))) }

# Mario template
n2pc_1_diss_wide = read_csv2("GA_P78_1_Diss_Raw Data_example.csv" %>% paste0("Grand Averages Robin_Mario/", .)) %>% 
  mutate(N2pc = contraP78 - ipsiP78) %>% select(c(1, 4:6)) %>% 
  rename(ipsilateral = ipsiP78, contralateral = contraP78)

n2pc_1_diss = n2pc_1_diss_wide %>% gather("ERP", "Voltage", 2:4)
n2pc_1_diss$ERP = factor(n2pc_1_diss$ERP, levels=c("contralateral", "ipsilateral", "N2pc")) #this order is important for the order in the legend


# N2pc Block 1
path.ga_1 = "N2pc_Block1_50Segm_only_compl_NA_AN_all_electrodes_modif.dat" %>% paste0("Generic Exports Block 1 and 2/", .)
n2pc_1_diss_wide = read.delim(path.ga_1, sep=" ", header = F) %>% t() %>% row_to_names(row_number = 1) %>% as.data.frame
rownames(n2pc_1_diss_wide) <-c(1:400)
vec <- c(-99:300) 
n2pc_1_diss_wide <- cbind(n2pc_1_diss_wide, Time = vec) #besser mit mutate(Time = vec) oder ganz fancy mutate(Time = 1:n() - 100)
n2pc_1_diss_wide <- n2pc_1_diss_wide[,c(11,15)] #besser mit select :)

n2pc_1_diss = n2pc_1_diss_wide %>% gather("ERP", "Voltage", 1)
n2pc_1_diss$ERP <- as.factor(n2pc_1_diss$ERP) 
n2pc_1_diss$Voltage <- as.numeric(n2pc_1_diss$Voltage) 

n2pc_1_diss = n2pc_1_diss %>% mutate(ERP = ifelse(ERP %>% grepl("P78", ., fixed=T), "P78", "PO78")) #better readability of legend


# N2pc Block 1 plot -------------------------------------------------------------
n2pc_1_diss %>% ggplot(aes(x=Time, y=Voltage, color=ERP, group=ERP)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  #geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  scale_color_manual(values=c("red", "blue", "black")) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(0.01, 0)) + ylab("N2pc (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(path.ga_1 %>% gsub(".csv", ".png", .)) #TODO add additional arguments from other script


# N2pc Block 2
path.ga_2 = "N2pc_Block2_50Segm_only_compl_NA_AN_all_electrodes_modif.dat" %>% paste0("Generic Exports Block 1 and 2/", .)
n2pc_2_diss_wide = read.delim(path.ga_2, sep=" ", header = F) %>% t() %>% row_to_names(row_number = 1) %>% as.data.frame
rownames(n2pc_2_diss_wide) <-c(1:400)
vec <- c(-99:300) 
n2pc_2_diss_wide <- cbind(n2pc_2_diss_wide, Time = vec) #besser mit mutate(Time = vec) oder ganz fancy mutate(Time = 1:n() - 100)
n2pc_2_diss_wide <- n2pc_2_diss_wide[,c(11,15)] #besser mit select :)

n2pc_2_diss = n2pc_2_diss_wide %>% gather("ERP", "Voltage", 1)
n2pc_2_diss$ERP <- as.factor(n2pc_2_diss$ERP) 
n2pc_2_diss$Voltage <- as.numeric(n2pc_2_diss$Voltage) 

n2pc_2_diss = n2pc_2_diss %>% mutate(ERP = ifelse(ERP %>% grepl("P78", ., fixed=T), "P78", "PO78")) #better readability of legend


# N2pc Block 2 plot -------------------------------------------------------------
n2pc_2_diss %>% ggplot(aes(x=Time, y=Voltage, color=ERP, group=ERP)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  #geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  scale_color_manual(values=c("red", "blue", "black")) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(0.01, 0)) + ylab("N2pc (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(path.ga_2 %>% gsub(".csv", ".png", .)) #TODO add additional arguments from other script


# Both blocks -------------------------------------------------------------

