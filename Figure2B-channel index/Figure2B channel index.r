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

######25-F:B######
fit<- read_excel("Channel.xlsx",1)
p25<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("F:B")+
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
  scale_y_continuous(limits = c(0.04,0.12), expand=c(0,0),breaks = c(0.04, 0.08, 0.12),
                     labels = label_number(accuracy = 0.04))
p25
dat<-ggplot_build(p25)$data[[1]]
p25a<-p25 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p25a
ggsave("p25-fungi_to_bacteria_ratio.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result25<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result25



######26-PCI######
fit<- read_excel("Channel.xlsx",2)
p26<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCI")+
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
  scale_y_continuous(limits = c(0,0.12), expand=c(0,0),breaks = c(0, 0.06, 0.12),
                     labels = label_number(accuracy = 0.06))

p26
dat <- ggplot_build(p26)$data[[1]]
p26a<- p26 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p26a
ggsave("p26-PCI.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result26<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result26


######27-NCR######
fit<- read_excel("Channel.xlsx",3)##
p27<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("NCR")+
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
  scale_y_continuous(limits = c(0.5,0.9), expand=c(0,0),breaks = c(0.5, 0.7, 0.9))
p27
dat <- ggplot_build(p27)$data[[1]]
p27a<- p27 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                          colour="white", size=0.5)
p27a
ggsave("p27-NCR.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result27<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result27


######28-NCI######
fit<- read_excel("Channel.xlsx",4)
p28<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size=0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("PCI")+
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
  scale_y_continuous(limits = c(0,40), expand=c(0,0),breaks = c(0, 20, 40),
                     labels = label_number(accuracy = 20))
p28
dat <- ggplot_build(p28)$data[[1]]
p28a<- p28 + geom_segment(data=dat,
                          aes(x=xmin, xend=xmax,
                              y=middle, yend=middle),
                              colour="white", size=0.5)
p28a
ggsave("p28-NCI.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result28<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result28

#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

