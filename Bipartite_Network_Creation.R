library(readr)
library(dplyr)
library(magrittr)
library(igraph)

all_wetlands<-read_csv("~/Wetlands/GIS_Wetlands_use_for_bipartite - Sheet1.csv")
all_wetlands<-na.omit(all_wetlands)

#delete columns we don't need
wetnet<-all_wetlands%>%select(Name, Management)
#create the network
#wetnet<-simplify(graph.edgelist(as.matrix(wetnet), directed=F))

#
x <- graph.data.frame(wetnet)
V(x)$type <- V(x)$name %in% wetnet[,1]
wetnet<-(get.adjacency(x))
wetnet<-as.matrix(wetnet)
wetnet<-as.data.frame(wetnet)

wets<-unique(all_wetlands$Management)

#Make the columns the orgs and the rows the wetlands
wetnet<-wetnet[-c(56:71),-c(1:55)]

#add the orgs that didnt give wetlands
soc_mat<-read_csv("~/Wetlands/Social_net.csv")



orgs<-which(soc_mat$X1 %in% wets==F)
orgs<-soc_mat[orgs,1]
x<-data.frame(matrix(0,ncol=nrow(orgs),nrow=nrow(wetnet)))
names(x)<-as.vector(orgs$X1)
wetnet<-cbind(wetnet,x)

#Alphabatize the rows and columns
wetnet <- wetnet[ order(row.names(wetnet)), order(names(wetnet))]
names(wetnet)[18]= "Confederated_Salish_and_Kootenai_Tribes._Fisheries_Program_Pablo"
write.csv(wetnet, file="bipartite_network_as.df.csv")
row.names(wetnet)<-NULL
names(wetnet)<-NULL
wetnet<-as.matrix(wetnet)
write.csv(wetnet, file="bipartite_network_MPNET_READY.csv")

