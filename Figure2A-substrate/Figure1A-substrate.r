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

######p1-TOC######
data1 <- read_excel("1_Substrate.xlsx",1)
head(data1)
p1<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "TOC (g kg-1 soil)",
             legend = "right")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(5,30),breaks = c(5,17.5,30))

p1
ggsave("p1-TOC.pdf", dpi=1000, width=45,height =45, units="mm")


######T test######
Result1<-compare_means(value~Treatment, data=data1, method = "t.test",
                  paired = FALSE, group.by = "Time")
Result1
#write.table(Result1,"ttest1.txt",col.names = NA, sep = '\t', quote = FALSE)


######p2-DOC######
data1 <- read_excel("1_Substrate.xlsx",2)
head(data1)
p2<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "DOC (g kg-1 soil)",
             legend = "right")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
   theme(axis.line.x.bottom = element_line(color = 'black'),
         axis.line.y.left   = element_line(color = 'black'),
         axis.line.y.right  = element_line(color = 'black'),
         axis.text.y.right  = element_blank(),
         axis.ticks.y.right = element_blank(),
         panel.border       = element_blank())+
   scale_y_continuous(expand = c(0,0),limits = c(0,1),breaks = c(0,0.5,1))

p2
ggsave("p2-DOC.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result2<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result2


######p3-CKMnO4######
data1 <- read_excel("1_Substrate.xlsx",3)
head(data1)
p3<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL ,ylab = "CKMnO4 (g kg-1 soil)",
             legend = "right")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,5),breaks = c(0,2.5,5))
p3
ggsave("p3-CKMnO4.pdf", dpi=1000, width=45,height =45, units="mm")

######T test######
Result4<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result4


######p4-TN######
data1 <- read_excel("1_Substrate.xlsx",4)
head(data1)
p4<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "TN (g kg-1 soil)",legend = "right")+
   theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
   theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
   theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
   scale_y_continuous(expand = c(0,0),limits = c(0.5,2),breaks = c(0.5,1,1.5,2))
p4
ggsave("p4-TN.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result4<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result4


######p5-DN######
data1 <- read_excel("1_Substrate.xlsx",5)
head(data1)
p5<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "DN (g kg-1 soil)",
             legend = "right")+
    theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
    theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
    theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
    scale_y_continuous(expand = c(0,0),limits = c(0.05,0.2),breaks = c(0.05,0.1,0.15,0.2))
p5
ggsave("p5-DN.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result5<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result5


######p6-DOCDN######
data1 <- read_excel("1_Substrate.xlsx",6)
head(data1)
p6<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "DOC:DN",
             legend = "right")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,10),breaks = c(0,5,10))
p6
ggsave("p6-DOCDN.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result6<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result6


######p7-TOCTN######
data1 <- read_excel("1_Substrate.xlsx",7)
head(data1)
p7<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL ,ylab = "TOC:TN",
             legend = "right")+
     theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
     theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
     theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
     scale_y_continuous(expand = c(0,0),limits = c(5,25),breaks = c(5,10,15,20,25))
p7
ggsave("p7-TOCTN.pdf", dpi=1000, width=45,height =45, units="mm")

######T test######
Result7<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result7


######p8-DOCTOC######
data1 <- read_excel("1_Substrate.xlsx",8)
names(data1)
p8<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "DOC:TOC (%)",legend = "right")+
     theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
     theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
     theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
      scale_y_continuous(expand = c(0,0),limits = c(2,4),breaks = c(2,3,4))
p8
ggsave("p8-DOCTOC.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result8<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result8



######p9-CKMnO4TOC######
data1 <- read_excel("1_Substrate.xlsx",9)
head(data1)
p9<-ggviolin(data1, x="Time", y="value", color="Treatment",
             fill="Treatment",palette =c("#D73027","#4575B4"),
             add = "boxplot",add.params = list(color="white"),
             xlab =NULL , ylab = "CKMnO4:TOC (%)",
             legend = "right")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),
        axis.line.y.right  = element_line(color = 'black'),
        axis.text.y.right  = element_blank(),
        axis.ticks.y.right = element_blank(),
        panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(5,25),breaks = c(5,10,15,20,25))
p9
ggsave("p9-CKMnO4TOC.pdf", dpi=1000, width=45,height =45, units="mm")
######T test######
Result9<-compare_means(value~Treatment, data=data1, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result9

#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

