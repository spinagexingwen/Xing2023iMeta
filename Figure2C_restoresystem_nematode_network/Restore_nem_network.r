setwd(readClipboard())
getwd()

library(Hmisc)
library(igraph)
library(phyloseq)
library(ggplot2)
library(info.centrality)
library(export)

nem <- read.delim('1_restore_nem_table.txt', row.name = 1, check.names = FALSE)
dim(nem)
#nem <- nem[which(rowMeans(nem) > 0.0001), ]
nem <- t(nem)

#######Correlation analysis########
nem_corr <- rcorr(nem, type = 'spearman')
r <- nem_corr$r
#r[abs(r) < 0.6] <- 0
p <- nem_corr$P
#p <- p.adjust(p, method = 'BH')
p[p>=0.05] <- -1
p[p<0.05 & p>=0] <- 1
p[p==-1] <- 0

#######Correlation matrix########
z <- r * p
diag(z) <- 0
head(z)[1:6,1:6]
write.table(data.frame(z, check.names = FALSE), '2_restore_nem_corr.matrix.txt',
            col.names = NA, sep = '\t', quote = FALSE)
#######network_adjacency_matrix########
igraph <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
igraph
vcount(igraph)
igraph <- delete.vertices(igraph, names(degree(igraph)[degree(igraph) == 0]))
vcount(igraph)
E(igraph)$correlation <- E(igraph)$weight
E(igraph)$weight <- abs(E(igraph)$weight)
adj_matrix <- as.matrix(get.adjacency(igraph, attr = 'correlation'))
write.table(data.frame(adj_matrix, check.names = FALSE), '3_restore_nem_network.adj_matrix.txt', col.names = NA, sep = '\t', quote = FALSE)

########add characters for nodes#######
tax <- read.delim('4_taxonomy.txt', row.name = 1,check.names = FALSE, stringsAsFactors = FALSE)
head(tax)
tax <- tax[as.character(V(igraph)$name), ]
head(tax)
V(igraph)$Phylum <- tax$Phylum
V(igraph)$Abundance <- tax$Abundance


edge <- data.frame(as_edgelist(igraph))
edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(igraph)$weight,
  correlation = E(igraph)$correlation)
head(edge_list)
write.table(edge_list, '5_network.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

node_list <- data.frame(
  label = names(V(igraph)),
  Phylum = V(igraph)$Phylum,
  Abundance = V(igraph)$Abundance)
head(node_list)
write.table(node_list, '6_network.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

######then, the data from '5_network.edge_list.txt' and '6_network.node_list.txt' were used to get network plot in Gephi software.######
write.graph(igraph, 'gephi_network.graphml', format = 'graphml')
write.graph(igraph, 'cytoscape_network.gml', format = 'gml')



##############subnetwork#####################################
setwd(readClipboard())
getwd()
initial_adjacency<-read.delim('1_restore_nem_network.adj_matrix.txt',
                              row.names=1,sep='\t',check.names=FALSE)#
initial_adjacency[initial_adjacency<0]<-1
initial_adjacency[initial_adjacency>0]<-1
write.table(data.frame(initial_adjacency,check.names=FALSE),'adjacency_unweighted.txt',col.names=NA,sep='\t',quote=FALSE)
initial_abundance<-read.delim('2_restore_nem_abundance.txt',row.names=1,sep='\t',check.names=FALSE)#
head(initial_abundance)
initial_abundance[initial_abundance>0]<-1
write.table(data.frame(initial_abundance,check.names=FALSE),'adjacency_abundance.txt',col.names=NA,sep='\t',quote=FALSE)


adjacency_unweight<-read.delim('adjacency_unweighted.txt',row.names=1,sep='\t',check.names=FALSE)
head(adjacency_unweight)[1:6]
g <- graph_from_adjacency_matrix(as.matrix(adjacency_unweight),mode = 'undirected', weighted = NULL, diag = FALSE)
g
otu<-read.delim('adjacency_abundance.txt',row.names=1,sep='\t',check.names=FALSE)
head(otu)[1:6]

sub_graph <- list()
for (i in names(otu)) {
  sample_i <- otu[i]
  select_node <- rownames(sample_i)[which(sample_i != 0)]
  sub_graph[[i]] <- subgraph(g, select_node)}
sub_graph
sub_graph$`restore6_1`##example

plot_network(g)
plot_network(sub_graph$`restore6_1`)
sample_name <- c()
nodes_num <- c()
degree <- c()
average_path_length <- c()
betweenness_centralization <- c()
clustering_coefficient <- c()
connectance <- c()
modularity <- c()

for(i in 1:length(sub_graph)){
  sample_name <- c(sample_name, names(sub_graph[i]))
  nodes_num <- c(nodes_num, length(V(sub_graph[[i]])))
  degree <- c(degree, mean(degree(sub_graph[[i]])))
  clustering_coefficient <- c(clustering_coefficient, transitivity(sub_graph[[i]]))
  average_path_length <- c(average_path_length, average.path.length(sub_graph[[i]], directed = FALSE))
  connectance<- c(connectance, edge_density(sub_graph[[i]],loops=FALSE))
  modularity<- c(modularity, modularity(sub_graph[[i]], membership(cluster_fast_greedy(sub_graph[[i]]))))
  betweenness_centralization <- c(betweenness_centralization, centralization.betweenness(sub_graph[[i]])$centralization)
}

sub_graph_stat <- data.frame(nodes_num, degree,
                             clustering_coefficient,
                             average_path_length,
                             connectance,
                             modularity,
                             betweenness_centralization)

rownames(sub_graph_stat) <- sample_name
head(sub_graph_stat)
write.csv(sub_graph_stat, 'restore_nem_subnetwork_indices.csv', quote = FALSE)

adj_matrix <- as.matrix(get.adjacency(igraph))
write.table(data.frame(adj_matrix, check.names = FALSE),
            'Subnetwork_Restore6_1.txt', col.names = NA, sep = '\t', quote = FALSE)


########prepared the initial abundance table for sample restore6-1#####

nem <- read.delim('restore6-1-abundance.txt', row.name = 1, check.names = FALSE)
nem <- t(nem)
genus_corr <- rcorr(nem, type = 'spearman')
r <- genus_corr$r
r[abs(r) < 0.60] <- 0
p <- genus_corr$P
#p <- p.adjust(p, method = 'BH')
p[p>=0.05] <- -1
p[p<0.05& p>=0] <- 1
p[p==-1] <- 0
z <- r * p
diag(z) <- 0
head(z)[1:6,1:6]

igraph <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
igraph

igraph <- delete.vertices(igraph, names(degree(igraph)[degree(igraph) == 0]))
vcount(igraph)
E(igraph)$correlation <- E(igraph)$weight
E(igraph)$weight <- abs(E(igraph)$weight)
adj_matrix <- as.matrix(get.adjacency(igraph, attr = 'correlation'))
write.table(data.frame(adj_matrix, check.names = FALSE), 'restore6-1-subnetwork.adj_matrix.txt', col.names = NA, sep = '\t', quote = FALSE)


edge <- data.frame(as_edgelist(igraph))
edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(igraph)$weight,
  correlation = E(igraph)$correlation
)
head(edge_list)

write.table(edge_list, '!restore6-1-network.edge_list.txt',
            sep = '\t', row.names = FALSE, quote = FALSE)
######and then we calculated the numbers of positive or negative links in Excel##


##########robustness calculation########
network.raw<- as.matrix(read.delim('restore6-1-subnetwork.adj_matrix.txt', row.name = 1, check.names = FALSE))
network.raw[1:3,1:3]
otutab<-read.table("restore6-1-abundance.txt",header = T,row.names=1,sep="\t")
comm<-t(otutab)
sp.ra<-colMeans(comm)
head(sp.ra)
rand.remov.once<-function(netRaw, rm.percent, sp.ra, abundance.weighted=T){
  id.rm<-sample(1:nrow(netRaw), round(nrow(netRaw)*rm.percent))
  net.Raw=netRaw
  net.Raw[id.rm,]=0;  net.Raw[,id.rm]=0;
  if (abundance.weighted){
    net.stength= net.Raw*sp.ra
  } else {
    net.stength= net.Raw
  }

  sp.meanInteration<-colMeans(net.stength)
  id.rm2<- which(sp.meanInteration<=0)
  remain.percent<-(nrow(netRaw)-length(id.rm2))/nrow(netRaw)
  remain.percent
}

rm.p.list=seq(0.05,0.2,by=0.05)
rmsimu<-function(netRaw, rm.p.list, sp.ra, abundance.weighted=T,nperm=100){
  t(sapply(rm.p.list,function(x){
    remains=sapply(1:nperm,function(i){
      rand.remov.once(netRaw=netRaw, rm.percent=x, sp.ra=sp.ra, abundance.weighted=abundance.weighted)
    })
    remain.mean=mean(remains)
    remain.sd=sd(remains)
    remain.se=sd(remains)/(nperm^0.5)
    result<-c(remain.mean,remain.sd,remain.se)
    names(result)<-c("remain.mean","remain.sd","remain.se")
    result
  }))
}

Weighted.simu<-rmsimu(netRaw=network.raw, rm.p.list=seq(0.05,1,by=0.05), sp.ra=sp.ra, abundance.weighted=T,nperm=100)
Unweighted.simu<-rmsimu(netRaw=network.raw, rm.p.list=seq(0.05,1,by=0.05), sp.ra=sp.ra, abundance.weighted=F,nperm=100)

dat1<-data.frame(nemportion.removed=rep(seq(0.05,1,by=0.05),2),rbind(Weighted.simu,Unweighted.simu),
                 weighted=rep(c("weighted","unweighted"),each=20),
                 biota=rep("restore-nem",40),treat=rep("restore",40))
currentdat<-dat1
currentdat[1:10,1:7]
write.csv(currentdat,"6-1-restore_nem_random_removal_result.csv")


#######Vulnerability#########
g = graph_from_adjacency_matrix(as.matrix(read.delim('restore6-1-subnetwork.adj_matrix.txt',
                                                     row.name = 1, check.names = FALSE)),
                                mode="upper", weighted = NULL,
                                diag = FALSE, add.colnames = NULL)
iso_node_id = which(degree(g)==0)
g2 = delete.vertices(g, iso_node_id)
length(V(g2));length(E(g2))
node.vul<-info.centrality.vertex(g2)
write.csv(node.vul,"6-1-node.vul.csv")
max(node.vul)


#########keystone taxa#########
initial_adjacency<-read.delim('restore6-1-subnetwork.adj_matrix.txt',
                              row.names=1,sep='\t',check.names=FALSE)#
initial_adjacency[initial_adjacency<0]<-1
initial_adjacency[initial_adjacency>0]<-1
write.table(data.frame(initial_adjacency,check.names=FALSE),'adjacency_unweighted.txt',
            col.names=NA,sep='\t',quote=FALSE)
adjacency_unweight <- read.delim('adjacency_unweighted.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(adjacency_unweight)[1:6]
igraph <- graph_from_adjacency_matrix(as.matrix(adjacency_unweight), mode = 'undirected', weighted = NULL, diag = FALSE)
igraph
V(igraph)$degree <- degree(igraph)
set.seed(123)
V(igraph)$modularity <- membership(cluster_fast_greedy(igraph))
nodes_list <- data.frame(
  nodes_id = V(igraph)$name,
  degree = V(igraph)$degree,
  modularity = V(igraph)$modularity
)
head(nodes_list)
write.table(nodes_list, '6-1-nodes_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

source('zi_pi.r')
adjacency_unweight <- read.delim('adjacency_unweighted.txt',
                                 row.names = 1, sep = '\t', check.names = FALSE)
nodes_list <- read.delim('6-1-nodes_list.txt', row.names = 1, sep = '\t', check.names = FALSE)
nodes_list <- nodes_list[rownames(adjacency_unweight), ]
zi_pi <- zi.pi(nodes_list, adjacency_unweight,
               degree = 'degree', modularity_class = 'modularity')
head(zi_pi)
write.table(zi_pi, '6-1-zi_pi_result.txt', sep = '\t', row.names = FALSE, quote = FALSE)


zi_pi <- na.omit(zi_pi)
zi_pi[which(zi_pi$within_module_connectivities < 2.5 & zi_pi$among_module_connectivities < 0.62),'type'] <- 'Peripherals'
zi_pi[which(zi_pi$within_module_connectivities < 2.5 & zi_pi$among_module_connectivities > 0.62),'type'] <- 'Connectors'
zi_pi[which(zi_pi$within_module_connectivities > 2.5 & zi_pi$among_module_connectivities < 0.62),'type'] <- 'Module hubs'
zi_pi[which(zi_pi$within_module_connectivities > 2.5 & zi_pi$among_module_connectivities > 0.62),'type'] <- 'Network hubs'

p1<-ggplot(zi_pi, aes(among_module_connectivities, within_module_connectivities)) +
  geom_point(aes(color = type), alpha = 0.5, size = 2) +
  scale_color_manual(values = c('gray','red','blue','purple'),
                     limits = c('Peripherals', 'Connectors', 'Module hubs', 'Network hubs'))+
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'),
        panel.background = element_blank(), legend.key = element_blank()) +
  labs(x = 'Among-module connectivities', y = 'Within-module connectivities', color = '') +
  geom_vline(xintercept = 0.62) +
  geom_hline(yintercept = 2.5)

p1

graph2ppt(file="6-1.pptx", width=5, height=3, append=TRUE)

