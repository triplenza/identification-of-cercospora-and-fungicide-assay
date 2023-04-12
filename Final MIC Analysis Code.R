#Date: 7/12/2022
#Author: Nik
#Title: MIC Fungicide Sensitivity Assay

rm(list = ls(all=TRUE)) # removes all variables in the global environment so you start fresh
options(scipen = 999) # stops anything from being in scientific notation
setwd(dir="D:/MSU/Master/Research/Fungicide Sensitivity Testing/First Trial Code/")

##function to check, then load any packages that we desire
ipak <- function( pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("broom","stringr", "tidyverse","ggplot2", "drc","lattice","car", "lme4", "lsmeans", "plyr", "plotrix", "knitr", "ggplot2", "lmtest", "lmerTest", "Rmisc", "gridExtra", "plotly", "webshot", "ggpmisc", "ggsci","scales","RColorBrewer", "cowplot")
ipak(packages)

##Load data

FlutMycelialMIC<-read_delim(file = "03312022 Firsttrial MIC.csv",delim = ",", na="")
ID<- read_delim(file = "03312022 First Trial Info.csv",delim = ",",na="")
IsolateData<-unique(FlutMycelialMIC$Isolate)
FlutAll<-left_join(FlutMycelialMIC,ID, by="Isolate")
FlutAll$Year <- factor(FlutAll$Year)  #set as factor

View(FlutAll)


###Summary statistics 
FlutAll %>% 
  group_by(Species) %>%
  summarise(count = count(MIC_ppm))

##ggplot 

#Fifure 3.4 Compare MIC between species
bar1<- ggplot(FlutAll, aes(x=MIC_ppm, fill=Species, color = Species)) +
       geom_bar(width=0.5) +
       #geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25) +
       #ggtitle("Minimum Inhibitory Concentration for Different Species") +
       xlab("Flutriafol Concentration (µg/mL)") +
       ylab("Number of Isolates") +
       scale_x_discrete(limits = c("1", "2.5", "5", "10", "20", ">20")) + #rearrange x axis
       guides(fill=guide_legend(title="Species"))+
  scale_fill_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  scale_color_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  theme(legend.position = c(0.65,0.7),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 16, family = "serif"))
  
#Compare between fungicide history
bar2<- ggplot(FlutAll, aes(x=MIC_ppm, fill=Fungicide_History)) +
       geom_bar(position = "dodge", width=0.7) +
       #ggtitle("Minimum Inhibitory Concentration Based on Fungicide History") +
       xlab("Flutriafol Concentration (µg/mL)") +
       ylab("Number of Isolates") +
       scale_x_discrete(limits = c("1", "2.5", "5", "10", "20", ">20")) + #rearrange x axis
       scale_fill_manual('Fungicide History', values = c("coral2", "steelblue", "orange", "gray")) +
  theme(legend.position = c(0.7,0.7),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 16, family = "serif"))

#Compare between geographic location
bar3<- ggplot(FlutAll, aes(x=MIC_ppm, fill=Region)) +
       geom_bar(position = "dodge", width=0.7) +
       #ggtitle("Minimum Inhibitory Concentration based on Geographic Location") +
       xlab("Flutriafol Concentration (µg/mL)") +
       ylab("Number of Isolates") +
       scale_x_discrete(limits = c("1", "2.5", "5", "10", "20", ">20"))+
       scale_fill_manual('Region', values=c('coral2','steelblue', 'orange', 'darkgreen')) +
  theme(legend.position = c(0.7,0.7),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 16, family = "serif"))

#Compare by year
bar4<- ggplot(FlutAll, aes(x=MIC_ppm, fill=Year)) +
       geom_bar(position = "dodge", width=0.7) +
       #ggtitle("Minimum Inhibitory Concentration for Different Year") +
       xlab("Flutriafol Concentration (µg/mL)") +
       ylab("Number of Isolates") +
       scale_x_discrete(limits = c("1", "2.5", "5", "10", "20", ">20")) + #rearrange x axis
       scale_fill_manual('Year', values=c('coral2','steelblue')) +
  theme(legend.position = c(0.7,0.7),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"),
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 16, family = "serif"))

#Overall 
bar5<- ggplot(FlutAll, aes(x=MIC_ppm)) +
       geom_bar(position = "dodge", width=0.7) +
       #ggtitle("Minimum Inhibitory Concentration") +
       xlab("Flutriafol Concentration (µg/mL)") +
       ylab("Number of Isolates") +
       scale_x_discrete(limits = c("1", "2.5", "5", "10", "20", ">20"))  #rearrange x axis
  
ggsave(bar1, filename = "MICspecies.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(bar2, filename = "MICfungicide.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(bar3, filename = "MICregion.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(bar4, filename = "MICyear.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(bar5, filename = "MICAll.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
