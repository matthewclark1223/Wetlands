library(readr)
library(dplyr)
library(magrittr)
library(igraph)
IS1KM<- read_csv("~/Wetlands/Wetland Connectivity/Intersect1KM.csv")
IS5KM<- read_csv("~/Wetlands/Wetland Connectivity/Intersect_5KM.csv")
IS10KM<- read_csv("~/Wetlands/Wetland Connectivity/Intersect_10KM.csv")
IS20KM<- read_csv("~/Wetlands/Wetland Connectivity/Intersect_20KM.csv")
all_wetlands<-read_csv("~/Wetlands/GIS_Wetlands_2 - Sheet1.csv")
all_wetlands<-na.omit(all_wetlands)
#create a function to delete the name overlaps
del_overlap<-function(x){
  x[-c(which(x$Name == x$Name_1)),]
}


IS1KM<-del_overlap(IS1KM)
IS5KM<-del_overlap(IS5KM)
IS10KM<-del_overlap(IS10KM)
IS20KM<-del_overlap(IS20KM)

#Make these into edge lists by removing the extra columns
IS1KM%<>%select(Name, Name_1)
IS5KM%<>%select(Name, Name_1)
IS10KM%<>%select(Name, Name_1)
IS20KM%<>%select(Name, Name_1)

#read these into igraph and turn them into adjacency matices
#One km first
one<-simplify(graph.edgelist(as.matrix(IS1KM), directed=F))
one<-(get.adjacency(one))
one<-as.matrix(one)
one<-as.data.frame(one)
#add the wetlands that dont have 1KM connections
non_con_1<-which(all_wetlands$Name %in% names(one)==F)
non_con1<-all_wetlands[non_con_1,1]
x<-data.frame(matrix(0,ncol=nrow(non_con1),nrow=length(one)))
names(x)<-non_con1$Name
one<-cbind(one,x)
rows1<-which(names(one) %in% row.names(one)==F)
rows1<-names(one[rows1])
y<-data.frame(matrix(0,ncol=ncol(one),nrow=length(rows1)))
names(y)<-names(one)
row.names(y)<-rows1
one<-rbind(one,y)
one <- one[ order(row.names(one)), order(names(one))]
write.csv(one,file="~/Wetlands/Wetland Connectivity/Adj_mat_1KM.csv")

#five km
five<-simplify(graph.edgelist(as.matrix(IS5KM), directed=F))
five<-(get.adjacency(five))
five<-as.matrix(five)
five<-as.data.frame(five)
#add the wetlands that dont have 5KM connections
non_con_5<-which(all_wetlands$Name %in% names(five)==F)
non_con5<-all_wetlands[non_con_5,1]
x5<-data.frame(matrix(0,ncol=nrow(non_con5),nrow=length(five)))
names(x5)<-non_con5$Name
five<-cbind(five,x5)
rows5<-which(names(five) %in% row.names(five)==F)
rows5<-names(five[rows5])
y5<-data.frame(matrix(0,ncol=ncol(five),nrow=length(rows5)))
names(y5)<-names(five)
row.names(y5)<-rows5
five<-rbind(five,y5)
five <- five[ order(row.names(five)), order(names(five))]
write.csv(five,file="~/Wetlands/Wetland Connectivity/Adj_mat_5KM.csv")

#ten km
ten<-simplify(graph.edgelist(as.matrix(IS10KM), directed=F))
ten<-(get.adjacency(ten))
ten<-as.matrix(ten)
ten<-as.data.frame(ten)
#add the wetlands that dont have 10KM connections
non_con_10<-which(all_wetlands$Name %in% names(ten)==F)
non_con10<-all_wetlands[non_con_10,1]
x10<-data.frame(matrix(0,ncol=nrow(non_con10),nrow=length(ten)))
names(x10)<-non_con10$Name
ten<-cbind(ten,x10)
rows10<-which(names(ten) %in% row.names(ten)==F)
rows10<-names(ten[rows10])
y10<-data.frame(matrix(0,ncol=ncol(ten),nrow=length(rows10)))
names(y10)<-names(ten)
row.names(y10)<-rows10
ten<-rbind(ten,y10)
ten <- ten[ order(row.names(ten)), order(names(ten))]
write.csv(ten,file="~/Wetlands/Wetland Connectivity/Adj_mat_10KM.csv")


#twenty km
twent<-simplify(graph.edgelist(as.matrix(IS20KM), directed=F))
twent<-(get.adjacency(twent))
twent<-as.matrix(twent)
twent<-as.data.frame(twent)
#add the wetlands that dont have 20KM connections
non_con_20<-which(all_wetlands$Name %in% names(twent)==F)
non_con20<-all_wetlands[non_con_20,1]
x20<-data.frame(matrix(0,ncol=nrow(non_con20),nrow=length(twent)))
names(x20)<-non_con20$Name
twent<-cbind(twent,x20)
rows20<-which(names(twent) %in% row.names(twent)==F)
rows20<-names(twent[rows20])
y20<-data.frame(matrix(0,ncol=ncol(twent),nrow=length(rows20)))
names(y20)<-names(twent)
row.names(y20)<-rows20
twent<-rbind(twent,y20)
twent <- twent[ order(row.names(twent)), order(names(twent))]
write.csv(twent,file="~/Wetlands/Wetland Connectivity/Adj_mat_20KM.csv")

