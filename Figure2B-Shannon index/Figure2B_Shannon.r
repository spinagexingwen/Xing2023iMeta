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

########################################
######boxplots##########################
########################################

######p20-Shannon index of bacteria######
fit<- read_excel("Shannon.xlsx",1)##
p20<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Shannon index")+
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
  scale_y_continuous(limits = c(6,12), expand=c(0,0),breaks = c(6, 9, 12),
                     labels = label_number(accuracy = 3))
p20
dat <- ggplot_build(p20)$data[[1]]
p20a<- p20 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p20a
ggsave("p20-bacteria_shannon_index.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result20<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result20



######p21-Shannon index of fungi######
fit<- read_excel("Shannon.xlsx",2)##
p21<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Shannon index")+
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
  scale_y_continuous(limits = c(5,9), expand=c(0,0),breaks = c(5, 7, 9))
p21
dat <- ggplot_build(p21)$data[[1]]
p21a<- p21+geom_segment(data=dat,
                       aes(x=xmin, xend=xmax,
                           y=middle, yend=middle),
                           colour="white", size=0.5)
p21a
ggsave("p21-fungi_shannon_index.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result21<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result21



######p22-Shannon index of protozoa######
fit<- read_excel("Shannon.xlsx",3)##
p22<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+

  xlab("") +  ylab("Shannon index")+
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
  scale_y_continuous(limits = c(1,5), expand=c(0,0),
                     breaks = c(1, 3, 5),
                     labels = label_number(accuracy = 1))
p22
dat <- ggplot_build(p22)$data[[1]]
p22a<- p22 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p22a
ggsave("p22-protozoa_shannon_index.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result22<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result22



######p23-Shannon index of nematode######
fit<- read_excel("Shannon.xlsx",4)##
p23<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+

  xlab("") +  ylab("Shannon index")+
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
  scale_y_continuous(limits = c(2,4), expand=c(0,0),breaks = c(2, 3, 4),
                     labels = label_number(accuracy = 1))
p23
dat <- ggplot_build(p23)$data[[1]]
p23a<- p23 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p23a
ggsave("p23-nematode_shannon_index.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result23<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result23

######the calculation of average shannon index (average z-score) for whole microbiota community######
fit<- read_excel("Shannon.xlsx",5)
fit1 <- scale(fit[2:5])
fit2<- cbind(fit,rowMeans(fit1))
names(fit2)[6]<-paste("Whole_microbiota")
fit2
write.csv(fit2,"Average z-scores for whole microbiota community.csv")



######p24-Shannon index (average z score) of whole microbiota community######
fit<- read_excel("Shannon.xlsx",6)##
p24<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Average z score")+
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
  scale_y_continuous(limits = c(-2,2), expand=c(0,0),breaks = c(-2, 0, 2),
                     labels = label_number(accuracy = 1))
p24
dat <- ggplot_build(p24)$data[[1]]
p24a<- p24 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p24a
ggsave("p24-whole_microbiota__shannon_index.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result24<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result24

#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

