if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

se = function(x, na.rm = FALSE) { sd(x, na.rm) / sqrt(if(!na.rm) length(x) else length(na.omit(x))) }

path = "C:/Users/mar84qk/Dropbox/Arbeit/Attentional Bias Modification/3 Auswertung/"
#path = "F:/Dropbox/Arbeit/Attentional Bias Modification/3 Auswertung/"
#path = path %>% sub("F:", "D:", ., fixed=TRUE) #@home
path.ga_1 = "GA_P78_1_Diss_Raw Data.csv" %>% paste0(path, "EEG export/Grand Averages/", .)
path.gas = "GA_N2pc_1-3_Raw Data.csv" %>% paste0(path, "EEG export/Grand Averages/", .)
path.ga_AN_NA = "GA_Diff_AN&NA_1_Raw Data.csv" %>% paste0(path, "EEG export/Grand Averages/", .)
path.ga_P78 = "GA_P78_AN&NA_1-3_Raw Data.csv" %>% paste0(path, "EEG export/Grand Averages/", .)

n2pc_1_diss_wide = read.csv2(path.ga_1) %>% 
  mutate(N2pc = contraP78 - ipsiP78) %>% select(c(1, 4:6)) 
#names(n2pc_1_diss_wide) = names(n2pc_1_diss_wide) %>% gsub("P78", "", ., fixed=T)
names(n2pc_1_diss_wide) = c("Time", "ipsilateral", "contralateral", "N2pc")

n2pc_1_diss = n2pc_1_diss_wide %>% gather("ERP", "Voltage", 2:4)
n2pc_1_diss$ERP = factor(n2pc_1_diss$ERP, levels=c("contralateral", "ipsilateral", "N2pc")) #this order is important for the order in the legend



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



# n2pc over time ----------------------------------------------------------
n2pc.gas.wide = read.csv2(path.gas)
#names(n2pc.gas.wide) = names(n2pc.gas.wide) %>% gsub("N2pc_", "", ., fixed=T)
names(n2pc.gas.wide) = c("Time", 1:3 %>% paste0(". Session"), "HEOG_1")
n2pc.gas = n2pc.gas.wide %>% gather("Session", "Voltage", 2:4)
n2pc.gas %>% ggplot(aes(x=Time, y=Voltage, color=Session, group=Session)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  scale_color_manual(values=c("black", "red", "blue")) +
  #scale_color_manual(values=c("grey60", "black", "red", "blue")) + #if HEOG is included
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

#ggsave(path.gas %>% gsub(".csv", ".png", .))




# HEOG --------------------------------------------------------------------
n2pc.gas.wide %>% ggplot(aes(x=Time, y=HEOG_1)) + 
  #geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  #geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  #scale_color_manual(values="black") +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(2.5, 0)) + ylab("HEOG (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.box.margin=margin(rep(15, times=4)))


# n2pc time x training ---------------------------------------------------
time.window = "early"
n2pc.label = "N2pc (µV)"
if (time.window=="late") n2pc.label = n2pc.label %>% gsub("N2pc", "late N2pc", ., fixed=T)
n2pcXcondition.ga %>% filter(N2pc.time==time.window) %>% 
  ggplot(aes(x=Session, y=N2pc.m, colour=Condition)) + #the colour attribute implicitly creates a group=Condition. As soon as it is overwritten (cp. geom_errorbar), the position dodge is lost => explicitly call group=Condition there
  geom_hline(yintercept=0, linetype="dotted") + #linetype="dotted" vs. linetype="longdash"
  geom_line(size=2, position=dodge) +
  geom_errorbar(aes(ymin=N2pc.m-N2pc.se, ymax=N2pc.m+N2pc.se, group=Condition), width=.1, position=dodge, colour="black") +
  #geom_errorbar(data=n2pcXcondition.ga.within %>% filter(N2pc.time==time.window), mapping=aes(ymin=N2pc.m-N2pc.se, ymax=N2pc.m+N2pc.se, group=Condition), width=2, position=dodge, linetype="dashed") + #x=Session is inherited - if n2pc.ga.within$Session doesn't exist, call it explicitly
  geom_errorbar(data=n2pcXcondition.ga.within %>% filter(N2pc.time==time.window), mapping=aes(ymin=N2pc.m-N2pc.se, ymax=N2pc.m+N2pc.se, group=Condition), width=.1, position=dodge) + #x=Session is inherited - if n2pc.ga.within$Session doesn't exist, call it explicitly
  geom_point(size=3, shape=21, fill="white", position=dodge) +
  scale_colour_manual("Training", values=c("red", "blue")) +
  #scale_color_hue(l=55) + # Use darker colors with white background, l parameter = lightness (default is 65)
  scale_x_continuous(breaks=1:3) + xlab("Session") +
  scale_y_reverse() + ylab(n2pc.label) + theme_bw() +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        #legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(time.window %>% paste0(path, "../4 Ergebnisse/Dissertation/N2pc ", ., " x Training 2.png"))



# n2pc AN vs NA (1st measure) ---------------------------------------------
n2pc.gas.anna.wide = read.csv2(path.ga_AN_NA) %>% mutate(N2pc_1 = (N2pc_AN_1 + N2pc_NA_1) / 2) %>% select(Time, N2pc_1, everything())
#names(n2pc.gas.anna.wide) = c("Time", 1:3 %>% paste0(". Session"))
n2pc.gas.anna = n2pc.gas.anna.wide %>% gather("Sub", "Voltage", 2:4)
n2pc.gas.anna %>% ggplot(aes(x=Time, y=Voltage, color=Sub, group=Sub)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) + 
  theme_bw() +
  scale_color_manual(values=c("black", "red", "blue"), 
                     #labels=expression("N2pc"["1"], "N2pc"["AN1"], "N2pc"["NA1"])) +
                     labels=c("N2pc (mean)", "N2pc angry left", "N2pc angry right")) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=20), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.text.align = 0,
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(path.ga_AN_NA %>% gsub(".csv", ".png", .))



# n2pc P7 vs. P8 (1st measure) -------------------------------------------------
n2pc.gas.P78.wide = read.csv2(path.ga_P78)

n2pc.gas.P78.wide_1 = n2pc.gas.P78.wide %>% select(Time, ends_with("_1"))
n2pc.gas.P78_1 = n2pc.gas.P78.wide_1 %>% gather("Elektrode", "Voltage", -1)
n2pc.gas.P78_1$Elektrode = n2pc.gas.P78_1$Elektrode %>% as.factor()

lvls = n2pc.gas.P78_1$Elektrode %>% levels
n2pc.gas.P78_1 %>% ggplot(aes(x=Time, y=Voltage, color=Elektrode, group=Elektrode)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) +
  theme_bw() +
  scale_color_manual(values=c("blue4", "red4", "red", "deepskyblue3"), #in order of the factor levels: P7AN, P7NA, P8AN, P8NA
                     breaks=c(lvls[3], lvls[4], lvls[2], lvls[1]), #new order: P8AN, P8NA, P7NA, P7AN (descending amplitude of N2pc early)
                     #labels=expression("P8"["AN"], "P8"["NA"], "P7"["NA"], "P7"["AN"])) + #in new order
                     labels=expression("P8"["contra"], "P8"["ipsi"], "P7"["contra"], "P7"["ipsi"])) + #in new order
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=20), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        legend.text.align = 0,
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(path.ga_P78 %>% gsub(".csv", ".png", .))



# electrodes over time ------------------------------------------------
n2pc.gas.P78.wide = read.csv2(path.ga_P78)
n2pc.gas.P78 = n2pc.gas.P78.wide %>% gather("temp", "Voltage", -1)

split = n2pc.gas.P78$temp %>% strsplit("_")
n2pc.gas.P78$Elektrode = split %>% lapply('[', 1) %>% unlist() %>% as.factor()
n2pc.gas.P78$Konstellation = split %>% lapply('[', 2) %>% unlist() %>% as.factor()
n2pc.gas.P78$Gesicht = if_else(n2pc.gas.P78$Elektrode=="P8", n2pc.gas.P78$Konstellation %>% substring(1, 1), 
                               n2pc.gas.P78$Konstellation %>% substring(2, 2)) %>% as.factor
n2pc.gas.P78$Session = split %>% lapply('[', 3) %>% unlist() %>% as.factor()
n2pc.gas.P78 = n2pc.gas.P78 %>% select(-one_of("temp"))

lims = with(n2pc.gas.P78, c(max(Voltage), min(Voltage)))


# P7
elec = "P7"
n2pc.gas.P78 %>% filter(Elektrode==elec) %>% ggplot(aes(x=Time, y=Voltage, color=interaction(Gesicht, Session), group=interaction(Gesicht, Session))) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) +
  theme_bw() +
  scale_color_manual(values=c("red4", "blue4", "red", "blue", "orangered", "deepskyblue3"), #in order of the factor levels: A.1, N.1, A.2, N.2, A.3, N.3
                      breaks=c("A.1", "N.1", "A.2", "N.2", "A.3", "N.3"), #new order
                      #labels=bquote(expression(.(elec)["AN1"], .(elec)["AN2"], .(elec)["AN3"], .(elec)["NA1"], .(elec)["NA2"], .(elec)["NA3"]))) + #doesn't work
                      labels=expression("P7"["NA1"], "P7"["AN1"], "P7"["NA2"], "P7"["AN2"], "P7"["NA3"], "P7"["AN3"])) + #in new order
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  #scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  scale_y_reverse(limits=lims, expand=c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(elec %>% paste0(path, "../4 Ergebnisse/Dissertation/N2pc Electrodes ", ., ".png"))


# P8
elec = "P8"
n2pc.gas.P78 %>% filter(Elektrode==elec) %>% ggplot(aes(x=Time, y=Voltage, color=interaction(Gesicht, Session), group=interaction(Gesicht, Session))) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) +
  theme_bw() +
  scale_color_manual(values=c("red4", "blue4", "red", "blue", "orangered", "deepskyblue3"), #in order of the factor levels: A.1, N.1, A.2, N.2, A.3, N.3
                     breaks=c("A.1", "N.1", "A.2", "N.2", "A.3", "N.3"), #new order
                     #labels=bquote(expression(.(elec)["AN1"], .(elec)["AN2"], .(elec)["AN3"], .(elec)["NA1"], .(elec)["NA2"], .(elec)["NA3"]))) + #doesn't work
                     labels=expression("P8"["AN1"], "P8"["NA1"], "P8"["AN2"], "P8"["NA2"], "P8"["AN3"], "P8"["NA3"])) + #in new order
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  #scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  scale_y_reverse(limits=lims, expand=c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave(elec %>% paste0(path, "../4 Ergebnisse/Dissertation/N2pc Electrodes ", ., ".png"))



# Hemisphere Sub-N2pcs (P7 vs. P8) ----------------------------------------
n2pc.gas.P78.bias = n2pc.gas.P78 %>% select(-one_of("Konstellation")) %>% spread(Gesicht, Voltage) %>% mutate(Voltage=A-N)

lims.P78bias = with(n2pc.gas.P78.bias, c(max(Voltage), min(Voltage)))

# P7
elec = "P7"
n2pc.gas.P78.bias %>% filter(Elektrode==elec) %>% ggplot(aes(x=Time, y=Voltage, color=Session, group=Session)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) +
  theme_bw() +
  scale_color_manual(values=c("black", "chartreuse4", "purple"), labels=expression("N2pc"["P7, 1"], "N2pc"["P7, 2"], "N2pc"["P7, 3"])) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  #scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  scale_y_reverse(limits=lims.P78bias, expand=c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=20), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.box.margin=margin(rep(15, times=4)))
#ggsave(elec %>% paste0(path, "../4 Ergebnisse/Dissertation/N2pc Bias ", ., ".png"))

elec = "P8"
n2pc.gas.P78.bias %>% filter(Elektrode==elec) %>% ggplot(aes(x=Time, y=Voltage, color=Session, group=Session)) + 
  geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
  geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
  #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
  geom_line(size=2) +
  theme_bw() +
  scale_color_manual(values=c("black", "chartreuse4", "purple"), labels=expression("N2pc"["P8, 1"], "N2pc"["P8, 2"], "N2pc"["P8, 3"])) +
  scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
  #scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
  scale_y_reverse(limits=lims.P78bias, expand=c(0.01, 0)) + ylab("Voltage (µV)") +
  theme(text=element_text(size=20), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.box.margin=margin(rep(15, times=4)))
#ggsave(elec %>% paste0(path, "../4 Ergebnisse/Dissertation/N2pc Bias ", ., ".png"))

# temp = rowMeans(cbind(n2pc.gas.P78.bias %>% filter(Session==1 & Elektrode=="P7") %>% .$Voltage, n2pc.gas.P78.bias %>% filter(Session==1 & Elektrode=="P8") %>% .$Voltage))
# (temp - n2pc.gas %>% filter(Session=="1. Session") %>% .$Voltage) %>% mean #identical except for rounding errors

# n2pc.gas.P78.bias %>% ggplot(aes(x=Time, y=Voltage, color=Session, group=Session)) + 
#   geom_rect(xmin=180, xmax=300, ymin=-Inf, ymax=Inf, fill="grey75", colour="white", show.legend=F) + #colour sets colour of border! Use NA to remove it
#   geom_rect(xmin=300, xmax=400, ymin=-Inf, ymax=Inf, fill="grey85", colour="white", show.legend=F) +
#   geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0), linetype=2) +
#   #geom_line(size=2.5, colour="white") + #draw a border around the lines (looks horrible)
#   geom_line(size=2) +
#   theme_bw() +
#   scale_color_manual(values=c("black", "chartreuse4", "purple"), labels=expression("N2pc"["P8, 1"], "N2pc"["P8, 2"], "N2pc"["P8, 3"])) +
#   scale_x_continuous(expand = c(0, 0)) + xlab("Trial Time (ms)") +
#   #scale_y_reverse(expand = c(0.01, 0)) + ylab("Voltage (µV)") +
#   scale_y_reverse(limits=lims.P78bias, expand=c(0.01, 0)) + ylab("Voltage (µV)") +
#   facet_wrap(~Elektrode) +
#   theme(text=element_text(size=20), #default text size
#         #legend.text=element_text(size=20), #legend text size (except for legend title)
#         legend.title=element_blank(), #remove legend title
#         #legend.key = element_rect(size = 5),
#         legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
#         legend.background = element_rect(color="black"),
#         legend.justification = c(0, 1),
#         legend.position = c(0, 1),
#         legend.box.margin=margin(rep(15, times=4)))
# #ggsave(paste0(path, "../4 Ergebnisse/Dissertation/N2pc Bias P7 vs. P8.png"))


# absolute RTs ------------------------------------------------------------
rt.abs %>% filter(Included_RT==1) %>% ggplot(aes(x=Session, y=RT, colour=Emotion)) + #, shape=Condition
  scale_color_manual(values=c("#FF0000", "#0000FF"), labels = c("Angry", "neutral")) +
  geom_boxplot(size=2, outlier.size=2) + xlab("Session") + ylab("ReaktionsTrial Time (ms)") +
  theme_bw() + theme(text=element_text(size=30), #default text size
                     #legend.text=element_text(size=20), #legend text size (except for legend title)
                     #legend.title=element_blank(), #remove legend title
                     #legend.key = element_rect(size = 5),
                     legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
                     legend.background = element_rect(color="black"),
                     legend.justification = c(1, 1),
                     legend.position = c(1, 1),
                     legend.box.margin=margin(rep(15, times=4)))
#+ stat_summary(fun.y=mean, geom="point", shape=23, size=4, aes(color=Emotion)) #+ theme_classic()

#ggsave("RTs.png" %>% paste0(path, "../4 Ergebnisse/Dissertation/", .))

rt.detail %>% filter(Included_RT==1 & Emotion!="NN") %>% group_by(targetPos, Emotion, Condition) %>% summarise(RTmean=mean(RT, na.rm=T), RTse = se(RT, na.rm=T)) %>% 
  #ggplot(aes(x=targetPos, y=RTmean, colour=Emotion, shape=Condition, group=interaction(Condition, Emotion))) +
  ggplot(aes(x=targetPos, y=RTmean, colour=interaction(Condition, Emotion), group=interaction(Condition, Emotion))) +
  geom_line(size=2, position=dodge) + #aes(linetype=Condition),
  geom_errorbar(aes(ymin=RTmean-RTse, ymax=RTmean+RTse), width=.1, position=dodge, colour="black") +
  #geom_errorbar(data=n2pc.ga.within %>% filter(N2pc.time==time.window), mapping=aes(ymin=N2pc.m-N2pc.se, ymax=N2pc.m+N2pc.se, group=Condition), width=2, position=dodge, linetype="dashed") + #x=Session is inherited - if n2pc.ga.within$Session doesn't exist, call it explicitly
  #geom_errorbar(data=n2pc.ga.within %>% filter(N2pc.time==time.window), mapping=aes(ymin=N2pc.m-N2pc.se, ymax=N2pc.m+N2pc.se, group=Condition), width=.1, position=dodge) + #x=Session is inherited - if n2pc.ga.within$Session doesn't exist, call it explicitly
  geom_point(size=5, fill="white", position=dodge) +
  #scale_colour_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red4", "red", "blue4", "deepskyblue3"), labels=c("Angry & ABM", "Angry & Dot Probe", "Neutral & ABM", "Neutral & Dot Probe")) +
  #scale_color_hue(l=55) + # Use darker colors with white background, l parameter = lightness (default is 65)
  ylab("RT (ms)") + xlab("Zielreizposition") + labs(color='Emotion x Condition') + theme_bw() +
  scale_x_discrete(labels=c("links", "rechts")) +
  theme(text=element_text(size=30), #default text size
        #legend.text=element_text(size=20), #legend text size (except for legend title)
        #legend.title=element_blank(), #remove legend title
        #legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'), #increase legend symbols size (and thus vertical line spacing)
        legend.background = element_rect(color="black"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.box.margin=margin(rep(15, times=4)))

#ggsave("RT targetPos x Emotion x Training.png" %>% paste0(path, "../4 Ergebnisse/Dissertation/", .))
