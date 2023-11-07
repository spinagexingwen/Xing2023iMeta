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

######p15-bacteria gene copy######
fit<- read_excel("Abundance.xlsx",1)
p15<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Gene copies (× 10 7 g-1 soil)")+
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
  scale_y_continuous(limits = c(0,8), expand=c(0,0),breaks = c(0, 4, 8),
                     labels = label_number(accuracy = 4))
p15
dat <- ggplot_build(p15)$data[[1]]
p15a<- p15+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p15a
ggsave("p15-bacteria_gene_copy.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result15<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result15


######p16-fungi gene copy######
fit<- read_excel("abundance.xlsx",2)
p16<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Gene copies (× 10 6 g-1 soil)")+
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
  scale_y_continuous(limits = c(0,8), expand=c(0,0),breaks = c(0, 4, 8),
                     labels = label_number(accuracy = 4))
p16
dat <- ggplot_build(p16)$data[[1]]
p16a<- p16+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p16a
ggsave("p16-fungi_gene_copy.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result16<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result16


######p17-protozoa abundance######
fit<- read_excel("Abundance.xlsx",3)
p17<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Individuals (× 10 4 g-1 soil)")+
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
  scale_y_continuous(limits = c(0,4), expand=c(0,0),breaks = c(0, 2, 4),
                     labels = label_number(accuracy = 2))
p17
dat <- ggplot_build(p17)$data[[1]]
p17a<- p17+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p17a
ggsave("p17-protozoa_abundance.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result17<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result17


######p18-nematode abundance######
fit<- read_excel("Abundance.xlsx",4)
p18<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab("Individuals (× 100 g-1 soil)")+
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
  scale_y_continuous(limits = c(0,1200), expand=c(0,0),breaks = c(0, 600, 1200),
                     labels = label_number(accuracy = 600))
p18
dat <- ggplot_build(p18)$data[[1]]
p18a<- p18+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p18a
ggsave("p18-nematode abundance.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result18<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result18



######the calculation of average abundance (average z-score) for whole microbiota community######
fit<- read_excel("Abundance.xlsx",5)
fit1 <- scale(fit[2:5])
fit2<- cbind(fit,rowMeans(fit1))
names(fit2)[6]<-paste("Whole_microbiota")
fit2
write.csv(fit2,"Average z-scores for whole microbiota community.csv")



######p19-average abundance (z-score) for the whole microbiota community######
fit<- read_excel("Abundance.xlsx",6)
p19<-ggplot(fit, aes(x=Time, y=value, color=Treatment))+
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
  scale_y_continuous(limits = c(-3,3), expand=c(0,0),breaks = c(-3, 0, 3),
                     labels = label_number(accuracy = 1))
p19
dat <- ggplot_build(p19)$data[[1]]
p19a<- p19+ geom_segment(data=dat,
                         aes(x=xmin, xend=xmax,
                             y=middle, yend=middle),
                         colour="white", size=0.5)
p19a
ggsave("p19-whole-microbiota-abundance.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result19<-compare_means(value~Treatment, data=fit, method = "t.test",
                        paired = FALSE, group.by = "Time")
Result19


#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

