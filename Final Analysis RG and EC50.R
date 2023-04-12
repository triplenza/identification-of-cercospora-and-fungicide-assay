#Date: 7/12/2022
#Author: Nik
#Title: Analysis testing

##Figures, tables, analysis, and statistical tests for flutriafol baseline sensitivity paper 

setwd("D:/MSU/Master/Research/Fungicide Sensitivity Testing/First Trial Code/")

ipak <- function( pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("broom","ggmap","sf","stats","ggrepel","pwr","stringr", "tidyverse","ggplot2", "drc","lattice","car", "lme4", "lsmeans", "plyr", "plotrix", "knitr", "ggplot2", "lmtest", "lmerTest", "Rmisc", "gridExtra", "plotly", "webshot", "ggpmisc", "ggsci","scales", "wesanderson","RColorBrewer", "emmeans", "multcomp", "multcompView")
ipak(packages)
display.brewer.all(colorblindFriendly = TRUE)

ls()
rm(list = ls(all=TRUE))

##load data

EC50Mycelial<-read_delim(file = "AllEc50MYC 080722.csv",delim=",",na=".")
FlutInfo<-read_delim(file = "03312022 First Trial Info.csv",na="")
EC50All<-left_join(EC50Mycelial,FlutInfo, by="Isolate")
EC50All$Year <- factor(EC50All$Year)
EC50All$Fungicide_History <- factor(EC50All$Fungicide_History)
View(EC50All)


###Summary statistics 
EC50All %>% 
  group_by(Species) %>%
  summarise(count = n(),
            mean = mean(Estimate),
            median = median(Estimate),
            max = max(Estimate),
            min = min(Estimate),
            se = sd(Estimate)/sqrt(length(Estimate)))

# run ANOVA for EC50 value and write the outcome of the model fitting
ANOVA.EC50 = lm(Estimate ~ Species, data=EC50All )
#LSD
LSD.EC50 <- emmeans(ANOVA.EC50, ~Species, adjust='none')     #write the means in a separate data set
pairs(LSD.EC50, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.EC50,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 


###Visualize all factor in a ggplot


#Figure 3.7 Mean EC50 estimation by species
#plot by species
hist1<-
ggplot(EC50All, aes(x=Estimate, color=Species, fill=Species))+
  geom_histogram(binwidth=0.2,boundary=0,closed="left", alpha = 0.8)+ 
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Species), linetype="dashed", size = 1)+
  labs(x=expression(bold("Mean EC"[50]*" Estimation (µg/ml)")))+
  ylab("Number of Isolates")+
  #annotate("text", x=0.6, y=120, label="Mean =\n 0.346 µg/ml")+
  #annotate("text", x=2.0, y=30, label="Mean =\n 1.590 µg/ml")+
  guides(fill=guide_legend(title="Species"))+
  #scale_fill_discrete(labels = c("C. zeae-maydis (N=253)", "Cercospora spp. (N=87)"))+
  scale_fill_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  scale_color_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  theme(legend.position = c(0.8,0.5),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"))

#plot by region
hist2<-
  ggplot(EC50All, aes(x=Estimate, color=Region, fill=Region))+
  geom_histogram(binwidth = 0.2, alpha = 0.8)+ 
  labs(x=expression("Mean EC"[50]*" Estimation (µg/ml)"))+
  ylab("Number of Isolates")+
  guides(fill=guide_legend(title="Region"))+
  scale_fill_brewer(palette="Dark2")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = c(0.8,0.5),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"))

#plot by year
hist3<-
  ggplot(EC50All, aes(x=Estimate, color=Year, fill=Year))+
  geom_histogram(binwidth = 0.2, alpha = 0.8)+ 
  labs(x=expression("Mean EC"[50]*" Estimation (µg/ml)"))+
  ylab("Number of Isolates")+
  guides(fill=guide_legend(title="Year"))+
  scale_fill_brewer(palette="Dark2")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = c(0.8,0.5),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 15, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 15, face="bold", family = "serif"))

ggsave(hist1, filename = "species.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(hist2, filename = "region.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)
ggsave(hist3, filename = "year.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)


###Shapiro-Wilk Test for normality 
shapiro.test(EC50All$Estimate)

######FIGURES
#### Relative Growth
#load data for relative growth
MycelialFull<-read_delim(file = "Mycelial_processed.csv",delim=",",na=".")
FlutInfo<-read_delim(file = "03312022 First Trial Info.csv",na="")
MycelialAll<-left_join(MycelialFull,FlutInfo, by="Isolate")
MycelialFull$relative<-as.numeric(MycelialFull$relative)

    
#plot based on species
RGCzm <- MycelialAll[MycelialAll$Species == "C. zeae-maydis",]
RGCf <- MycelialAll[MycelialAll$Species == "C. cf. flagellaris",]
RGCk <- MycelialAll[MycelialAll$Species == "C. kikuchii",]
RGCM <- MycelialAll[MycelialAll$Species == "Cercospora sp. M",]
RGCT <- MycelialAll[MycelialAll$Species == "Cercospora sp. T",]
RGCQ <- MycelialAll[MycelialAll$Species == "Cercospora sp. Q",]

curve.RGCzm<-drm(100*relative~ ppm, data = RGCzm, fct = LL.4())
curve.RGCf<-drm(100*relative~ ppm, data = RGCf, fct = LL.3())
curve.RGCk<-drm(100*relative~ ppm, data = RGCk, fct = LL.3())
curve.RGCM<-drm(100*relative~ ppm, data = RGCM, fct = LL.3())
curve.RGCT<-drm(100*relative~ ppm, data = RGCT, fct = LL.3())
curve.RGCQ<-drm(100*relative~ ppm, data = RGCQ, fct = LL.3())

# Build a color palette
coul <- brewer.pal(n=6, "Set2")

plot3<-plot(curve.RGCf,
            xlab="Flutriafol Concentration (µg/ml)",
            ylab="Relative Growth (%)", 
            lty=1, lwd=2, col="#66C2A5", family = "serif", font.lab = 2, font.axis = 2,
            xlim=c(0,20), ylim = c(0,100), 
            cex.lab = 1.8, cex.axis = 1.8)
plot3<-plot(curve.RGCk, add = TRUE, lty=1, lwd=2, col="#FC8D62")
plot3<-plot(curve.RGCzm, add = TRUE, lty=1, lwd=2, col="#8DA0CB")
plot3<-plot(curve.RGCM, add = TRUE, lty=1, lwd=2, col="#E78AC3")
plot3<-plot(curve.RGCQ, add = TRUE, lty=1, lwd=2, col="#A6D854")
plot3<-plot(curve.RGCT, add = TRUE, lty=1, lwd=2, col="#FFD92F")


par(mar=c(5,6,4,1)+.1)  #ensure y axis label not cut off
par(family = "serif")   #set font type for legend
legend("bottomleft", legend = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"), 
       col = coul, lty = 1, lwd=2, cex = 1.5, text.font = 2)


####Relative Growth 
#subset data for 0.5 and 1 ppm, rename facet label
mycelial_subset <- filter(MycelialAll, ppm %in% c("0.5", "1")) %>% 
  mutate(ppm = dplyr::recode (ppm, 
                              '0.5' = "0.5 µg/ml",
                              '1' = "1 µg/ml")) %>% 
  group_by(Isolate, Species, ppm) %>%
  summarise(mean = mean(relative))

growth1<-
  ggplot(mycelial_subset, aes(x=mean, color=Species, fill=Species))+
  geom_histogram(binwidth=0.05,boundary=0,closed="left", alpha = 0.8)+ 
  xlab("Relative Growth at 0.5 µg/ml")+
  ylab("Number of Isolates")+
  #annotate("text", x=0.6, y=120, label="Mean =\n 0.346 µg/ml")+
  #annotate("text", x=2.0, y=30, label="Mean =\n 1.590 µg/ml")+
  facet_wrap(~ppm, nrow = 2) +
  guides(fill=guide_legend(title="Species"))+
  scale_fill_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  scale_color_brewer(palette="Dark2", labels = c("C. cf. flagellaris (N=77)", "C. kikuchii (N=4)", "C. zeae-maydis (N=253)", "Cercospora sp. M (N=2)", "Cercospora sp. Q (N=1)", "Cercospora sp. T (N=3)"))+
  theme(legend.position = c(0.8,0.3),
        axis.text.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"))

ggsave(growth1, filename = "RG at 0.5 and 1 ppm.png",  bg = "transparent", dpi=600,units="in", height=5, width=7)


#p-value and LSD calculation
# need to run ANOVA first and write the outcome of the model fitting
ANOVA.RG = lm(relative ~ Species, data=mycelial_subset )
#LSD
LSD.RG <- emmeans(ANOVA.RG, ~Species, adjust='none')     #write the means in a separate data set
pairs(LSD.RG, adjust='none')                                         #get p-values for all pair-wise comparisons
cld(LSD.RG,alpha=0.05,  Letters=letters,  adjust="none")             #get letters for mean separations 
