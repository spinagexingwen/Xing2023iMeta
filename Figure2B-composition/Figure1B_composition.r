setwd(readClipboard())
getwd()

library(vegan)
library(ape)

######principal coordinates analysis######
######bacteria######
spe<-read.csv("Bacteria_asv_table.csv",row.names=1,header=T)
spe<-t(spe)
spe.bray<- vegdist(spe,method="bray")
spe.b.pcoa<- pcoa(spe.bray)
summary(spe.b.pcoa)
write.csv(spe.b.pcoa$values,"Bac_pcoa$values.csv")
write.csv(spe.b.pcoa$vectors,"Bac_pcoa$vectors.csv")


######fungi######
spe<-read.csv("Fungi_asv_table.csv",row.names=1,header=T)
spe<-t(spe)
spe.bray<- vegdist(spe,method="bray")
spe.b.pcoa<- pcoa(spe.bray)
summary(spe.b.pcoa)
write.csv(spe.b.pcoa$values,"Fun_pcoa$values.csv")
write.csv(spe.b.pcoa$vectors,"Fun_pcoa$vectors.csv")


######protozoa######
spe<-read.csv("Protozoa_asv_table.csv",row.names=1,header=T)
spe<-t(spe)
spe.bray<- vegdist(spe,method="bray")
spe.b.pcoa<- pcoa(spe.bray)
summary(spe.b.pcoa)
write.csv(spe.b.pcoa$values,"Pro_pcoa$values.csv")
write.csv(spe.b.pcoa$vectors,"Pro_pcoa$vectors.csv")

######nematode######
spe<-read.csv("Nematode_genus_table.csv",row.names=1,header=T)
spe<-t(spe)
spe.bray<- vegdist(spe,method="bray")
spe.b.pcoa<- pcoa(spe.bray)
summary(spe.b.pcoa)
write.csv(spe.b.pcoa$values,"Nem_pcoa$values.csv")
write.csv(spe.b.pcoa$vectors,"Nem_pcoa$vectors.csv")



########################################
######boxplots##########################
########################################
library(readxl)
library(Rmisc)
library(ggplot2)
library(grid)
library(multcompView)
library(lsmeans)
library(agricolae)
library(scales)
library(ggpubr)
######p10-bacteria######
fit<- read_excel("Composition.xlsx",1)##
#fit$Treatment<-factor(fit$Treatment,levels=c("Arable system","Restored natural area"))
#fit$type<-factor(fit$type,levels=c("Bacteria"))
#fit$Time<-factor(fit$Time,levels=c("Jun.","Sep."))

p10<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCoa1 (63.4%)")+
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(limits = c(-0.5,1), expand=c(0,0),breaks = c(-0.5, 0, 0.5,1),
                     labels = label_number(accuracy = 0.5))

p10
dat<-ggplot_build(p10)$data[[1]]
p10a<-p10 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p10a
ggsave("p10-bacteria.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result10<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result10

######p11-fungi######
fit<- read_excel("composition.xlsx",2)##

p11<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCo1 (62.6%)")+
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(limits = c(-0.5,1), expand=c(0,0),breaks = c(-0.5, 0, 0.5,1),
                     labels = label_number(accuracy = 0.5))
p11
dat<-ggplot_build(p11)$data[[1]]
p11a<-p11 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p11a
ggsave("p11-fungi.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result11<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result11


######p12-protozoa######
fit<- read_excel("composition.xlsx",3)

p12<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCo1 (31.2%)")+
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(limits = c(-0.5,1), expand=c(0,0),breaks = c(-0.5, 0, 0.5,1),
                     labels = label_number(accuracy = 0.5))
p12
dat <- ggplot_build(p12)$data[[1]]
p12a<- p12 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p12a
ggsave("p12-protozoa.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result12<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result12


######p13-nematode######
fit<- read_excel("composition.xlsx",4)

p13<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCo1 (37.6%)")+
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(limits = c(-0.5,1), expand=c(0,0),breaks = c(-0.5, 0, 0.5,1),
                     labels = label_number(accuracy = 0.5))
p13
dat <- ggplot_build(p13)$data[[1]]
p13a<- p13 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p13a
ggsave("p13-nematode.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result13<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result13

######the calculation of PCO1 scores for whole microbiota community######
fit<- read_excel("composition.xlsx",5)
fit1<- cbind(fit,rowMeans(fit[3:6]))
names(fit1)[7]<-paste("Whole_microbiota")
fit1
write.csv(fit1,"PCO1 scores for whole microbiota community.csv")

######p14-whole micorbiota community######
fit<- read_excel("composition.xlsx",6)##

p14<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Average PCo1")+
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(limits = c(-0.5,1), expand=c(0,0),breaks = c(-0.5,0, 0.5,1),
                     labels = label_number(accuracy = 0.5))
p14
dat <- ggplot_build(p14)$data[[1]]
p14a<- p14+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p14a
ggsave("p14-whole_micorbiota.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result14<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result14
#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

