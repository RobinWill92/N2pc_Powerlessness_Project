library(tidyverse)
library(afex)

exclude = c(107, #too many triggers in block 1, no triggers in block 2
            151 #no EEG data for block 1
            )

se = function(x, na.rm = TRUE) { sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x))) }


# Valid Trials EEG --------------------------------------------------------
data_Block1 = read_csv("Grand Average_Block1__999segm.csv", col_names=F) %>% 
  filter(X1 != "at least 999 are required for processing.") %>% 
  separate(X1, into=c("subject", "valid"), sep="': ") %>% #pull(subject)
  mutate(subject = subject %>% str_extract("['][0-9]+") %>% gsub("'", "", .) %>% as.integer(),
         type = ifelse(valid %>% grepl("_NA", .), "NA", "AN"),
         valid = ifelse(valid %>% grepl("not found", .), 0, 
                        valid %>% gsub("P78", "", .) %>% gsub("[^0-9]", "", .) %>% as.integer()),
         block = 1) %>% 
  mutate(subjectCheck = paste0(subject, type)) %>% filter(subjectCheck %>% duplicated(fromLast=T) == F) %>% select(-subjectCheck) %>% 
  pivot_wider(names_prefix="valid_", names_from=type, values_from=valid)

data_Block2 = read_csv("Grand Average_Block2__999segm.csv", col_names=F) %>% 
  filter(X1 != "at least 999 are required for processing.") %>% 
  separate(X1, into=c("subject", "valid"), sep="': ") %>% #pull(subject)
  mutate(subject = subject %>% str_extract("['][0-9]+") %>% gsub("'", "", .) %>% as.integer(),
         type = ifelse(valid %>% grepl("_NA", .), "NA", "AN"),
         valid = ifelse(valid %>% grepl("not found", .), 0, 
                        valid %>% gsub("P78", "", .) %>% gsub("[^0-9]", "", .) %>% as.integer()),
         block = 2) %>% 
  mutate(subjectCheck = paste0(subject, type)) %>% filter(subjectCheck %>% duplicated(fromLast=T) == F) %>% select(-subjectCheck) %>% 
  pivot_wider(names_prefix="valid_", names_from=type, values_from=valid)

data_combined_Blocks = bind_rows(data_Block1, data_Block2) %>% arrange(subject, block); rm(data_Block1, data_Block2)

stages = read_csv("0 Overview.csv")
#setdiff(data_combined_Blocks$subject, stages$subject) #subjects 151, 295 not in stages but in data #fixed
#setdiff(stages$subject, data_combined_Blocks$subject) #subject 7777 not in data but in stages (test subject) => left_join
data_stages = data_combined_Blocks %>% left_join(stages, by="subject") %>% 
  mutate(block = block %>% as_factor(), stage = stage %>% as_factor())
#data_stages %>% filter(valid_NA > 100 | valid_AN > 100) #subject 107 has too many trials? => exclude (see top of script)
data_stages = data_stages %>% filter(subject %in% exclude == F)


#inclusion
data_stages %>% mutate(include_2x50 = valid_NA + valid_AN >= 50,
                       include_4x25 = valid_NA >= 25 & valid_AN >= 25) %>% 
  group_by(subject) %>% mutate(across(contains("include"), all)) %>% #only include if all blocks per subject are valid
  filter(include_2x50 != include_4x25)

data_stages = data_stages %>% 
  #mutate(include = valid_NA + valid_AN >= 50) %>% 
  mutate(include = valid_NA >= 25 & valid_AN >= 25) %>% 
  group_by(subject) %>% mutate(include = all(include)) #only include if all blocks per subject are valid
include = data_stages %>% filter(include) %>% pull(subject) %>% sort() %>% unique()


# Explore Inclusions ------------------------------------------------------
#inclusion by stage
data_stages %>% filter(block==1) %>% #both blocks are linked => only use block1 for calculation
  group_by(stage) %>%
  summarise(total_subjects = n(),
            valid_subjects = sum(include),
            valid_subjets_p = valid_subjects / n())
#data_stages %>% mutate(valid = rowMeans(tibble(valid_NA, valid_AN))) %>% 
#data_stages %>% mutate(valid = valid_NA + valid_AN, valid_p = valid / 160, stage = stage %>% as_factor()) %>% #na.omit() %>% 
data_stages %>% pivot_longer(valid_NA:valid_AN, names_to="distractors", values_to="valid") %>% 
  #filter(valid > 0) %>% 
  ggplot(aes(x=valid, fill=stage, color=include)) + 
  facet_grid(rows=vars(distractors), cols=vars(block), labeller="label_both")+
  geom_vline(xintercept = 25, linetype="dashed", color="red") +
  geom_histogram(breaks=seq.int(0, 80, 5)) +
  scale_fill_viridis_d() + theme_bw()

#valid trials for included subjects
data_stages %>% filter(include) %>% group_by(block) %>% 
  summarise(valid_NA = mean(valid_NA), valid_AN = mean(valid_AN),
            valid_NA_p = valid_NA / 80, valid_AN_p = valid_AN / 80)

#subjects to manually exclude from Grand Average
data_stages %>% filter(xor(valid_NA < 25, valid_AN < 25))
data_stages %>% 
  #ggplot(aes(x=valid_AN, y=valid_NA, shape=block, color=subject, group=subject)) +
  ggplot(aes(x=valid_AN, y=valid_NA, color=include, group=subject)) +
  geom_vline(xintercept = 25-.5, linetype="dashed", color="red") +
  geom_hline(yintercept = 25-.5, linetype="dashed", color="red") +
  geom_line() + geom_point(size=2) + theme_bw()


# Data --------------------------------------------------------------------
data_Block1 = read_delim("Area_Block1_all_partic_tw_110_210.txt") %>% 
  full_join(read_delim("Area_Block1_all_partic_Indiv_Ch_tw_110_210.txt", na = c("", "NA", "???")), by="Vp") %>% 
  mutate(Vp = Vp %>% gsub("_Task","",.) %>% gsub("Block1_2", "Block1", .)) %>% 
  mutate(Block = 1)

data_Block2 = read_delim("Area_Block2_all_partic_tw_110_210.txt") %>% 
  full_join(read_delim("Area_Block2_all_partic_Indiv_Ch_tw_110_210.txt", na = c("", "NA", "???")), by="Vp") %>% 
  mutate(Vp = ifelse(Vp %>% grepl("128_2", .), Vp %>% gsub("128_2", "128", .) %>% paste0("_2"), Vp),
         Block = 2)

dataEeg = data_Block1 %>% bind_rows(data_Block2) %>% 
  separate(Vp, into = c("Vp","Dot","Probe","BlockCheck","FileNum"), sep = "_") %>% 
  mutate(FileNum = ifelse(FileNum %>% is.na(), 1, FileNum) %>% as.integer(),
         Vp = Vp %>% as.integer()) %>% 
  select(Vp, Block, FileNum, Diff_P78_NA:`Contra_PO-Indiv_Ch_P78_AN_Even`) %>% select(-contains("PO")) %>% 
  arrange(Vp, Block)
names(dataEeg) = names(dataEeg) %>% gsub("-Indiv_Ch_P78", "", .); rm(data_Block1, data_Block2)


behavioral = read_csv("Dot_Probe_Task_Table_complete_24_08_22.csv") %>% 
  mutate(across(.fns=function(x) { ifelse(x %>% is.nan() | x == "NaN", NA, x) }))

#checks
#behavioral %>% filter({Vp_Manip != Vp_Block1 | Vp_Manip != Vp_Block2} %>% replace_na(T)) #no inconsistencies in subject number
#behavioral %>% filter({Condition != Cond_Post_Quest | Condition != Cond_Block1 | Condition != Cond_Block2} %>% replace_na(T)) #inconsistencies in conditions!
#behavioral %>% filter(Condition %>% is.na()) #but fixed in Condition column => drop rest

#some training trials are contained in block1
#behavioral %>% group_by(Vp_Manip) %>% summarise(N_block1 = Num_wrong_answ_Block1 + Num_corr_answ_Block1, N_block2 = Num_wrong_answ_Block2 + Num_corr_answ_Block2) %>% View("uneven N") #filter(N_block1 != N_block2)

#tidy
behavioral = behavioral %>% rename(Vp = Vp_Manip) %>% select(-contains("Vp_B")) %>% select(-contains("Cond_")) %>% 
  bind_rows(tibble(Vp = 152, Condition = 2)) %>% 
  mutate(Condition = case_when(Condition == 1 ~ "low power", 
                               Condition == 2 ~ "control", 
                               T ~ Condition %>% as.character()) %>% as_factor())
conditions = behavioral %>% select(Vp, Condition)


# Analysis N2pc -----------------------------------------------------------
#dataEeg %>% pull(Vp) %>% setdiff(conditions %>% pull(Vp))
n2pc = dataEeg %>% select(Vp, Block, contains("Diff_P78")) %>% 
  mutate(Diff_P78_all = (Diff_P78_NA + Diff_P78_AN)/2,
         Diff_P78_all_Odd = (Diff_P78_NA_Odd + Diff_P78_AN_Odd)/2,
         Diff_P78_all_Even = (Diff_P78_NA_Even + Diff_P78_AN_Even)/2) %>% 
  pivot_longer(-c("Vp", "Block"), values_to="n2pc") %>% 
  separate(name, into=c("diff", "topo", "distractors", "subset")) %>% mutate(subset = ifelse(subset %>% is.na(), "all", subset)) %>% select(-diff, -topo) %>% 
  left_join(conditions, by="Vp")

#ANOVA
n2pc.analysis = n2pc %>% filter(subset == "all", distractors != "all", Vp %in% include)
n2pc.analysis %>% afex::aov_ez(id="Vp", dv="n2pc",
                               within=c("Block", "distractors"),
                               between="Condition") %>% apa::anova_apa(force_sph_corr=T)

#plots
n2pc.analysis %>% group_by(Block, Condition, Vp) %>% summarise(n2pc = mean(n2pc)) %>% #one value per participant => correct calculation of standard error
  summarise(n2pc.se = se(n2pc), n2pc = mean(n2pc)) %>% 
  ggplot(aes(x = Block, y = n2pc, shape=Condition, group=Condition)) +
  geom_errorbar(aes(ymin=n2pc-n2pc.se, ymax=n2pc+n2pc.se)) +
  geom_line() + geom_point() + theme_bw()
n2pc.analysis %>% group_by(Block, distractors, Condition) %>% 
  summarise(n2pc.se = se(n2pc), n2pc = mean(n2pc)) %>% 
  ggplot(aes(x = Block, y = n2pc, color=distractors, shape=Condition, group=interaction(distractors, Condition))) +
  geom_errorbar(aes(ymin=n2pc-n2pc.se, ymax=n2pc+n2pc.se)) +
  geom_line() + geom_point() + theme_bw()


# Analysis contra ipsi ----------------------------------------------------
#the ANOVA & plots are not so important but the reliability estimates
contraipsi = dataEeg %>% select(Vp, Block, contains("Contra_"), contains("Ipsi_")) %>% 
  mutate(Contra_P78_all = (Contra_P78_NA + Contra_P78_AN)/2, Contra_P78_all_Odd = (Contra_P78_NA_Odd + Contra_P78_AN_Odd)/2, Contra_P78_all_Even = (Contra_P78_NA_Even + Contra_P78_AN_Even)/2,
         Ipsi_P78_all = (Ipsi_P78_NA + Ipsi_P78_AN)/2, Ipsi_P78_all_Odd = (Ipsi_P78_NA_Odd + Ipsi_P78_AN_Odd)/2, Ipsi_P78_all_Even = (Ipsi_P78_NA_Even + Ipsi_P78_AN_Even)/2) %>% 
  pivot_longer(-c("Vp", "Block"), values_to="voltage") %>% 
  separate(name, into=c("emotion", "topo", "distractors", "subset")) %>% mutate(subset = ifelse(subset %>% is.na(), "all", subset)) %>% select(-topo) %>% 
  left_join(conditions, by="Vp")

#ANOVA
contraipsi.analysis = contraipsi %>% filter(subset == "all", distractors != "all", Vp %in% include)
contraipsi.analysis %>% afex::aov_ez(id="Vp", dv="voltage",
                                     within=c("Block", "emotion", "distractors"),
                                     between="Condition") %>% apa::anova_apa(force_sph_corr=T)

#plots
# emotion (N2pc)
contraipsi.analysis %>% group_by(emotion, Vp) %>% summarise(voltage = mean(voltage)) %>% #one value per participant => correct calculation of standard error
  summarise(voltage.se = se(voltage), voltage = mean(voltage)) %>% 
  ggplot(aes(x = emotion, y = voltage)) +
  geom_errorbar(aes(ymin=voltage-voltage.se, ymax=voltage+voltage.se)) +
  geom_point() + theme_bw()

# emotion x distractors (hemisphere main effect phrased complicatedly :D, see below)
contraipsi.analysis %>% group_by(emotion, distractors, Vp) %>% summarise(voltage = mean(voltage)) %>% #one value per participant => correct calculation of standard error
  summarise(voltage.se = se(voltage), voltage = mean(voltage)) %>% 
  ggplot(aes(x = emotion, y = voltage, color=distractors, group=distractors)) +
  geom_errorbar(aes(ymin=voltage-voltage.se, ymax=voltage+voltage.se)) +
  geom_line() + geom_point() + theme_bw()



# Analysis Electrodes -----------------------------------------------------
electrodes = dataEeg %>% select(Vp, Block, contains("P7_"), contains("P8_")) %>% 
  mutate(P7_all = (P7_NA + P7_AN)/2, P7_all_Odd = (P7_NA_Odd + P7_AN_Odd)/2, P7_all_Even = (P7_NA_Even + P7_AN_Even)/2,
         P8_all = (P8_NA + P8_AN)/2, P8_all_Odd = (P8_NA_Odd + P8_AN_Odd)/2, P8_all_Even = (P8_NA_Even + P8_AN_Even)/2) %>% 
  pivot_longer(-c("Vp", "Block"), values_to="voltage") %>% 
  separate(name, into=c("hemisphere", "distractors", "subset")) %>% mutate(subset = ifelse(subset %>% is.na(), "all", subset)) %>% 
  left_join(conditions, by="Vp")

#ANOVA
electrodes.analysis = electrodes %>% filter(subset == "all", distractors != "all", Vp %in% include)
electrodes.analysis %>% afex::aov_ez(id="Vp", dv="voltage",
                               within=c("Block", "hemisphere", "distractors"),
                               between="Condition") %>% apa::anova_apa(force_sph_corr=T)

#plots
# hemisphere main effect
electrodes.analysis %>% group_by(hemisphere, Vp) %>% summarise(voltage = mean(voltage)) %>% #one value per participant => correct calculation of standard error
  summarise(voltage.se = se(voltage), voltage = mean(voltage)) %>% 
  ggplot(aes(x = hemisphere, y = voltage)) +
  geom_errorbar(aes(ymin=voltage-voltage.se, ymax=voltage+voltage.se)) +
  geom_point() + theme_bw()

# hemisphere x distractors (N2pc effect phrased complicatedly :D)
electrodes.analysis %>% group_by(hemisphere, distractors, Vp) %>% summarise(voltage = mean(voltage)) %>% #one value per participant => correct calculation of standard error
  summarise(voltage.se = se(voltage), voltage = mean(voltage)) %>% 
  mutate(emotion = ifelse(hemisphere=="P7" & distractors=="NA" | (hemisphere=="P8" & distractors=="AN"), "angry", "neutral")) %>% 
  ggplot(aes(x = hemisphere, y = voltage, color=emotion, shape=distractors, group=distractors)) +
  geom_errorbar(aes(ymin=voltage-voltage.se, ymax=voltage+voltage.se)) +
  geom_line(color="black") + geom_point() + theme_bw()



# Reliability -------------------------------------------------------------
n2pc %>% filter(subset != "all", Vp %in% include) %>% pivot_wider(names_from=subset, values_from=n2pc) %>% 
  group_by(Block, distractors) %>% summarise(r = cor.test(Odd, Even) %>% apa::cor_apa(r_ci=T, print=F))

contraipsi %>% filter(subset != "all", Vp %in% include) %>% pivot_wider(names_from=subset, values_from=voltage) %>% 
  group_by(Block, emotion, distractors) %>% summarise(r = cor.test(Odd, Even) %>% apa::cor_apa(r_ci=T, print=F))

electrodes %>% filter(subset != "all", Vp %in% include) %>% pivot_wider(names_from=subset, values_from=voltage) %>% 
  group_by(Block, hemisphere, distractors) %>% summarise(r = cor.test(Odd, Even) %>% apa::cor_apa(r_ci=T, print=F))