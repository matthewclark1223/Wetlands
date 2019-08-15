library(igraph)
library(readr)
library(statnet)
Social<- read.csv("~/Wetlands/Social_net.csv")
rownames(Social) <- Social[,1]
Social[,1] <- NULL
m<-as.matrix(Social)


g<-graph.adjacency(m,mode="undirected",weighted=NULL)
deg<-igraph::degree(g, mode="all")
layout <-layout.fruchterman.reingold(g)
plot(g, edge.color="#bababa", vertex.color=adjustcolor("#d53e4f", alpha.f=0.8),layout=layout,vertex.size=(deg*0.1+6),vertex.label=NA) 

layout<-layout_nicely(g)


Wets<-read.csv("~/Wetlands/Wetland Connectivity/Adj_mat_10KM.csv")
rownames(Wets) <- Wets[,1]
Wets[,1] <- NULL
m<-as.matrix(Wets)


g<-graph.adjacency(m,mode="undirected",weighted=NULL)
deg<-igraph::degree(g, mode="all")
l <- layout_with_kk(g)
plot(g, edge.color="#bababa", vertex.color=adjustcolor("#a6d96a", alpha.f=0.75), vertex.shape="square",latout=layout,vertex.size=(deg*0.3+6),vertex.label=NA) 









#try to import pnet network##Not Working 
readPNetStatistics <- function(filename)
{
  impordata <- scan(filename,what='character',quiet= TRUE)
  n <- as.numeric(impordata[(grep("*vertices",impordata)+1)])
  impordata <-
    impordata[(grep("*matrix",impordata)+1):(grep("*matrix",impordata)+n[1]
                                             *n[1])]
  AdjMatrix <- matrix(as.numeric(impordata),n,n,byrow=T)
  return(AdjMatrix)}
  
net <- readPNetStatistics("~/Wetlands/Switched_round_2_Network_M_0.txt") 



##Stay vs triangle hists
ExpectedStar<-rnorm(1000,141.1580, 11.364)
ExpectedTriangle<-rnorm(1000, 0.4340, 0.651)
ExpectedWetCon<-rnorm(1000, 48.6, 8.2)

h1<-hist(ExpectedStar,plot=FALSE)


h2<-hist(ExpectedTriangle,plot=FALSE )


h3<-hist(ExpectedWetCon,plot=FALSE)


plot(h1, main="Star Freq",cex.lab=2, cex.main=2,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,col="deepskyblue")
abline(v = 137,
       col = "red",
       lwd = 3)
box()


plot(h2, main="Triangle Freq",cex.lab=1.5, cex.main=1.5,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,xlim=c(-3,45),col="deepskyblue")
abline(v = 43,
       col = "red",
       lwd = 3)
box()



