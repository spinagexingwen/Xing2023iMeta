setwd(readClipboard())
getwd()

library(readxl)
library(Rmisc)
library(ggplot2)
library(grid)
library(multcompView)
library(lsmeans)
library(agricolae)
library(scales)
library(ggpubr)
library(vegan)
library(ape)

########################################
######boxplots##########################
########################################

######p1 BG:NAG######
fit<- read_excel("Enzymeindices.xlsx",2)
names(fit)
p1<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("BG:NAG")+
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
  scale_y_continuous(limits = c(0.6,1.8), expand=c(0,0),breaks = c(0.6, 1.2,1.8),
                     labels = label_number(accuracy = 0.6))
p1
dat <- ggplot_build(p1)$data[[1]]
p1a<- p1 + geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                       colour="white", size=0.5)
p1a
ggsave("1-BG_to_NAG_ratio.pdf", dpi=1000, height =45,width=45,units="mm")
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result1

######p2  BG:PHOX######
fit<- read_excel("Enzymeindices.xlsx",3)##
head(fit)
p2<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("BG:PHOX")+
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
  scale_y_continuous(limits = c(0.6,1.8), expand=c(0,0),breaks = c(0.6, 1.2,1.8),
                     labels = label_number(accuracy = 0.6))
p2
dat <- ggplot_build(p2)$data[[1]]
p2a<- p2 + geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                       colour="white", size=0.5)
p2a
ggsave("2-BG_to_PHOX_ratio.pdf", dpi=1000, height =45,width=45,units="mm")
######T test######
Result2<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result2


######p3  NAG:LAP######
fit<- read_excel("Enzymeindices.xlsx",4)##
head(fit)
p3<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("NAG:LAP")+
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
  scale_y_continuous(limits = c(0.3,1.5), expand=c(0,0),breaks = c(0.3, 0.9,1.5))
p3
dat <- ggplot_build(p3)$data[[1]]
p3a<- p3 + geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                       colour="white", size=0.5)
p3a
ggsave("p3-NAG_to_LAP_ratio.pdf", dpi=1000, height =45,width=45,units="mm")
######T test######
Result3<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result3



########p4 composition######
spe<-read.csv("enzyme_transformation.csv",row.names=1,header=T)
spe.bray<- vegdist(spe,method="bray")
spe.b.pcoa<- pcoa(spe.bray)
summary(spe.b.pcoa)
write.csv(spe.b.pcoa$values,"enzyme$values.csv")
write.csv(spe.b.pcoa$vectors,"enzyme$vectors.csv")



fit<- read_excel("Enzymeindices.xlsx",5)
head(fit)
p4<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Composition")+
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
  scale_y_continuous(limits = c(-0.4,0.4), expand=c(0,0),breaks = c(-0.4,-0.2, 0, 0.2, 0.4))
p4
dat <- ggplot_build(p4)$data[[1]]
p4a<- p4 + geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                       colour="white", size=0.5)
p4a
ggsave("p4-enzyme composition.pdf", dpi=1000, height =45,width=45,units="mm")
######T test######
Result4<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result4


######the calculation of Gross_activity######
fit<-read.csv("enzyme_transformation.csv",row.names=1,header=T)
fit1 <- scale(fit[1:8])
fit2<- cbind(fit,rowMeans(fit1))
names(fit2)[9]<-paste("Gross_activity")
fit2
write.csv(fit2,"Gross_activity.csv")

########p5 Gross_activity######
fit<- read_excel("Enzymeindices.xlsx",6)
p5<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Z score")+
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
  scale_y_continuous(limits = c(-2,3), expand=c(0,0),breaks = c(-2, -1, 0, 1, 2, 3))

p5
dat <- ggplot_build(p5)$data[[1]]
p5a<- p5 + geom_segment(data=dat,
                        aes(x=xmin, xend=xmax,
                            y=middle, yend=middle),
                        colour="white", size=0.5)
p5a
ggsave("p5-Gross_activity.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result5<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result5

##calculation of shannon index for total exoenzymes###
spe<-read.csv("enzyme_transformation.csv",row.names=1,header=T)
shannon_index <- diversity(spe, index = 'shannon', base = exp(1))
shannon_index
write.csv(shannon_index,"Shan_total_enzyme.csv")


########p6 Shan_total_enzyme######
fit<- read_excel("Enzymeindices.xlsx",7)
p6<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Diversity")+
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
scale_y_continuous(limits = c(2.04,2.10), expand=c(0,0),breaks = c(2.04, 2.07, 2.10))

p6
dat <- ggplot_build(p6)$data[[1]]
p6a<- p6 + geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                       colour="white", size=0.5)
p6a
ggsave("p6-Shan_total_enzyme.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result6<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result6

#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.



