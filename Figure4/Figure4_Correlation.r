setwd(readClipboard())
getwd()
library(psych)
library(readxl)
library(corrplot)
library(export)

###Figure3A##
x<- read_excel("biota.xlsx",1)
y<- read_excel("enzyme.xlsx",1)

corr_matrix<- corr.test(x[,-1], y[,-1], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
write.table(corr_matrix$r, 'Correlation_result_for_FigureA$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
write.table(corr_matrix$p, 'Correlation_result_for_FigureA$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)


col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
A<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
A
graph2ppt(file="Figure3_A.pptx", width=8, height=6, append=TRUE)


######Figure3B avgK######
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(2:6)], y[,2], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B1<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
            method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
            number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
            tl.srt = 45, cl.cex  = 0.8)
B1
graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)

###Figure3B avgCC##
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(7:11)], y[,3], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B2<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
B2
#graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)

###Figure3B GD##
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(12:16)], y[,4], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B3<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
B3
#graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)


###Figure3B Con##
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(17:21)], y[,5], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B4<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
B4
#graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)


###Figure3B modularity##
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(22:26)], y[,6], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B5<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
B5
#graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)


###Figure3B Robustness##
x<- read_excel("biota.xlsx",2)
y<- read_excel("enzyme.xlsx",2)
corr_matrix<- corr.test(x[,(27:31)], y[,7], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureB$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureB$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
B6<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
B6
#graph2ppt(file="Figure3_B.pptx", width=5, height=6, append=TRUE)


###Figure3C number of positive links##
x<- read_excel("biota.xlsx",3)
y<- read_excel("enzyme.xlsx",3)
corr_matrix<- corr.test(x[,(2:6)], y[,2], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureC$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureC$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
C1<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
C1
#graph2ppt(file="Figure3_C.pptx", width=5, height=6, append=TRUE)


###Figure3C number of negative links##
x<- read_excel("biota.xlsx",3)
y<- read_excel("enzyme.xlsx",3)
corr_matrix<- corr.test(x[,(7:11)], y[,2], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureC$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureC$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
C2<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
C2
#graph2ppt(file="Figure3_C.pptx", width=5, height=6, append=TRUE)


###Figure3C number of keystone taxa##
x<- read_excel("biota.xlsx",3)
y<- read_excel("enzyme.xlsx",3)
corr_matrix<- corr.test(x[,(12:16)], y[,2], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureC$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureC$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
C3<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
C3
#graph2ppt(file="Figure3_C.pptx", width=5, height=6, append=TRUE)


###Figure3C number of module##
x<- read_excel("biota.xlsx",3)
y<- read_excel("enzyme.xlsx",3)
corr_matrix<- corr.test(x[,(17:21)], y[,2], method = "spearman",adjust = "none")##spearman    pearson
corr_matrix$r
#write.table(corr_matrix$r, 'Correlation_result_for_FigureC$r.txt', sep = '\t', row.names = FALSE, quote = FALSE)
corr_matrix$p
#write.table(corr_matrix$p, 'Correlation_result_for_FigureC$p.txt', sep = '\t', row.names = FALSE, quote = FALSE)
col1 <- colorRampPalette(c("#D60047FF","white","#3B4992FF" ))
C5<-corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
             method = 'square',outline=FALSE, addCoef.col = 'grey26', col = col1(20),
             number.cex = 0.65, number.font=0.6, tl.cex =0.8,tl.col="black",
             tl.srt = 45, cl.cex  = 0.8)
C5
#graph2ppt(file="Figure3_C.pptx", width=5, height=6, append=TRUE)


#Note: Finally, all results showing in figures were visualized by Adobe Illustrator.

