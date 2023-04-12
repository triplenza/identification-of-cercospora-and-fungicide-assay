#Date: 6/21/2022
#Author: Nik
#Title: Analysis testing

##Figures, tables, analysis, and statistical tests for flutriafol baseline sensitivity paper 

setwd("D:/MSU/Master/Research/Fungicide Sensitivity Testing/Second Trial Code/")

ipak <- function( pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("broom","ggmap","sf","stats","ggrepel","pwr","stringr", "tidyverse","ggplot2", "drc","lattice","car", "lme4", "lsmeans", "plyr", "plotrix", "knitr", "ggplot2", "lmtest", "lmerTest", "Rmisc", "gridExtra", "plotly", "webshot", "ggpmisc", "ggsci","scales", "rstatix", "ggpubr")
ipak(packages)

ls()
rm(list = ls(all=TRUE))

##load data

EC50Mycelial_second<-read_delim(file = "AllEc50MYC.csv",delim=",",na=".")
EC50Mycelial_second$Set <- factor(EC50Mycelial_second$Set) #sets as factor
View(EC50Mycelial_second)

##Testing two means
#summary statistics
wilcox.test(Estimate ~ Set, data = EC50Mycelial_second, paired = TRUE)


#welch t-test
Set1 <- EC50Mycelial_second[EC50Mycelial_second$Set == "1",]
Set2 <- EC50Mycelial_second[EC50Mycelial_second$Set == "2",]

t.test(Set1$Estimate, Set2$Estimate)

##ggplot comparing btwn each set for each isolate
q <- ggplot(EC50Mycelial_second, aes(x=Isolate, y=Estimate, fill=Set)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar( aes(ymin=LowerBound, ymax=UpperBound), width=0.2, position = position_dodge(.9)) +
  ggtitle("Comparing EC50 Concentration by Isolate") +
  labs(y=expression(bold("EC"[50]*" Estimation (µg/ml)"))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10, face = "bold", family = "serif"),
          axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
          axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
          axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
          axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
          legend.text = element_text(size = 15, face = "bold", family = "serif"),
          legend.key = element_blank(),
          legend.title = element_text(size = 15, face="bold", family = "serif"))

ggsave(q, filename = "compareEC50.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)

#Summary statistics 
summary(EC50Mycelial$Estimate)
