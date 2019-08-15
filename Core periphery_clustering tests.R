library(statnet)
library(ggnetwork)
library(intergraph)
library(dplyr)
oc<-read.csv("~/Wetlands/Social_net.csv")
Soc2 <- Soc[,-1]
rownames(Soc2) <- Soc[,1]
Net<-as.matrix(Soc2)
Nodes<-read.csv("~/Wetlands/Social_Node_List - Sheet1.csv")

#Make a network object
Net<-as.network(x=Net, directed=F, loops=F, matrix.type="adjacency")
#Set vertx attributes
set.vertex.attribute(Net, "Type", as.character(Nodes$Type))
g<-asIgraph(Net)
#Community detection, core periphery tests!
#these are from http://pablobarbera.com/big-data-upf/html/02b-networks-descriptive-analysis.html
igraph::components(g)
#Make sure we are only looking at the giant connected component
giant <- igraph::decompose(g)[[1]]
#random walks
igraph::cluster_walktrap(giant)
#same thing but with 10 random walks...default is 4
igraph::cluster_walktrap(giant, steps=10)
#"fast and greedy" attempts to directly optimize
igraph::cluster_fast_greedy(giant)
#Edge betweenness iteritvely removes edges with high betweenness
igraph::cluster_edge_betweenness(giant)
#infomap attempts to map the flow of information
igraph::cluster_infomap(giant)
#attempts to label nodes and updated labels
igraph::cluster_label_prop(giant)


#lets plot some of the resulting communities this whole thin looks pretty terrible
comm <- igraph::cluster_fast_greedy(giant)
igraph::modularity(comm)
plot(comm, giant)
igraph::V(giant)$color<-igraph::membership(comm)
igraph::V(giant)$shape<-"circle"
igraph::V(giant)$shape=ifelse(igraph::V(giant)$Type == "Federal","rectangle",igraph::V(giant)$shape)
igraph::V(giant)$shape=ifelse(igraph::V(giant)$Type == "State","sphere",igraph::V(giant)$shape)
igraph::V(giant)$shape=ifelse(igraph::V(giant)$Type == "County","csquare",igraph::V(giant)$shape)
igraph::V(giant)$shape=ifelse(igraph::V(giant)$Type == "Tribal","vrectangle",igraph::V(giant)$shape)
igraph::V(giant)$shape=ifelse(igraph::V(giant)$Type == "Private","pie",igraph::V(giant)$shape)
plot( giant)


#k core decomposition
igraph::coreness(giant)
which(igraph::coreness(giant)==10)

library(igraph)
V(giant)$coreness <- coreness(giant)
par(mfrow=c(2, 5), mar=c(0.1,0.1,1,0.1))
set.seed(777); fr <- layout_with_fr(g)
for (k in 1:10){
  V(giant)$color <- ifelse(V(giant)$coreness>=k, "orange", "grey")
  plot(giant, main=paste0(k, '-core shell'), layout=layout)
}

layout<-layout_nicely(giant)
giant <- igraph::decompose(g)[[1]]
V(giant)$coreness <- coreness(giant)
par(mfrow=c(2, 2), mar=c(0.1,0.1,1,0.1))
igraph::V(giant)$color<-igraph::V(giant)$Type
layout<-layout_nicely(giant)
igraph::V(giant)$color<-"#bebada"
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Federal","#fb8072",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "State","#80b1d3",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "County","#8dd3c7",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Tribal","#fdb462",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Private","#b3de69",igraph::V(giant)$color)

a<-c(1,5,10)
for (k in a){
  V(giant)$color <- ifelse(V(giant)$coreness>=k, V(giant)$color,(adjustcolor(V(giant)$color, alpha.f = 0.001)))
  plot(giant, main=paste0(k, ' tie collaboration optimization'), layout=layout,vertex.label=NA)
}

#plot just the 10 core network
x<-which(coreness(giant) != 10)
core10whole<-igraph::delete.vertices(giant,c(x))
plot(core10whole, main=paste0(k,' tie collaboration optimization'),vertex.label=NA)
#legend
legend("bottomleft", legend=levels(as.factor(V(giant)$Type))  , 
       col = c("#8dd3c7","#fb8072","#bebada","#b3de69","#80b1d3","#fdb462") , bty = "n", pch=20 , 
       pt.cex = 3, cex = 1.5, text.col="black" , horiz= F, inset = c(-0.3, 0.3))



#Remove top 3 nodes and look at groups
#3     Army_Corps_of_Engineers_Missoula                                Federal    4935.65332730
#127   Montana_Watershed_Coordination_Council_Helena                  NonProfit  2465.91272427
#115   Montana_Department_of_Transportation_Wetland_Mitigation_Helena State       1822.25411710

grm<-igraph::delete.vertices(g,c(3,127))
components(grm)
plot(grm)
giant <- igraph::decompose(grm)[[1]]
plot(giant)
igraph::cluster_infomap(giant)
comm <- igraph::cluster_infomap(grm)
igraph::modularity(comm)
#
layout<-layout_nicely(grm)
igraph::V(grm)$color<-"#bebada"
igraph::V(grm)$color=ifelse(igraph::V(grm)$Type == "Federal","#fb8072",igraph::V(grm)$color)
igraph::V(grm)$color=ifelse(igraph::V(grm)$Type == "State","#80b1d3",igraph::V(grm)$color)
igraph::V(grm)$color=ifelse(igraph::V(grm)$Type == "County","#8dd3c7",igraph::V(grm)$color)
igraph::V(grm)$color=ifelse(igraph::V(grm)$Type == "Tribal","#fdb462",igraph::V(grm)$color)
igraph::V(grm)$color=ifelse(igraph::V(grm)$Type == "Private","#b3de69",igraph::V(grm)$color)
plot( grm,vertex.label=NA)
#

V(giant)$coreness <- coreness(giant)
par(mfrow=c(2, 2), mar=c(0.1,0.1,1,0.1))
igraph::V(giant)$color<-igraph::V(giant)$Type
layout<-layout_nicely(giant)
igraph::V(giant)$color<-"#bebada"
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Federal","#fb8072",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "State","#80b1d3",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "County","#8dd3c7",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Tribal","#fdb462",igraph::V(giant)$color)
igraph::V(giant)$color=ifelse(igraph::V(giant)$Type == "Private","#b3de69",igraph::V(giant)$color)

a<-c(1,3,6,9)
for (k in a){
  V(giant)$color <- ifelse(V(giant)$coreness>=k, V(giant)$color,(adjustcolor(V(giant)$color, alpha.f = 0.15)))
  plot(giant, main=paste0(k, ' tie optimization'), layout=layout,vertex.label=NA)
}

coreness(giant)
x<-which(coreness(giant) != 8)
core9rm<-igraph::delete.vertices(giant,c(x))
plot(core9rm)
