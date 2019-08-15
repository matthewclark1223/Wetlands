library(statnet)
library(ggnetwork)
library(intergraph)
library(dplyr)
devtools::install_github("briatte/ggnet")
library(ggnet)
Soc<-read.csv("~/Wetlands/Social_net.csv")
Soc2 <- Soc[,-1]
rownames(Soc2) <- Soc[,1]
Net<-as.matrix(Soc2)
Nodes<-read.csv("~/Wetlands/Social_Node_List - Sheet1.csv")

#Make a network object
Net<-as.network(x=Net, directed=F, loops=F, matrix.type="adjacency")
#Set vertx attributes
set.vertex.attribute(Net, "Type", as.character(Nodes$Type))
#Check it out
summary.network(Net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)


y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 5, 6, 8, 2) ]
names(y)=levels(Nodes$Type)
ggnet2(Net, color="Type",palette = y, alpha=0.75, size=4, edge.alpha=0.5,
       edge.color = c("color", "grey50"))



#try some basic ergms
mod<-ergm(Net~edges+nodematch("Type", diff=T))
summary(mod)

mod<-ergm(Net~kstar(10,  levels=NULL))
summary(mod)

#get some closeness metrics
g<-asIgraph(Net)
bet<-igraph::centr_betw(g, directed=F,normalized = F)
bdf<-cbind(Nodes,bet)
z<-bdf[,c(1,2,5)]
z<-z[order(-z$res),]
d<-bdf%>%group_by(Type)%>%summarise(avgb=median(res))


#Get n edges for each org
igraph::degree(g)
cbind(names(Soc[,-1]),igraph::degree(g))

#mlergm gof
y<-as.network(Net, directed = F)
x<-mlnet(y,node_memb = as.character(Nodes$Type))
model_est <- mlergm(x ~ edges, options = set_options(number_cores = 4), verbose = 0, seed = 123)
summary(model_est)
gof_res <- gof(model_est, options = set_options(number_cores = 4))
plot(gof_res, cutoff = 15, pretty_x = TRUE)

#Do something with Node Removal


#Make a histogram of the betweenness centralities 
ggplot(z,aes(x=res))+geom_density( aes(y=..scaled..,fill=Type), alpha=0.6)+
  scale_x_sqrt(limits=c(NA,5500),breaks=c(5000,2500,1000,500,100,0.5),
               labels=c(5000,2500,1000,500,100,0.5),expand=expand_scale(mult=0.005))+
  scale_y_continuous(expand=expand_scale(mult=0.005))+
  geom_vline(data=z, aes(xintercept=median(res)), color="black",
             linetype="dashed", size=2)+
  xlab("Betweenness Centrality")+
  scale_fill_manual(values=c("#8dd3c7","#fb8072","#bebada","#ffffb3","#80b1d3","#fdb462"))+
  ylab("Density of Nodes")+
  theme_classic()+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20, color="black"), 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())




#no color
ggplot(z,aes(x=res))+geom_density( fill="skyblue")+
   scale_x_sqrt(limits=c(NA,5500),breaks=c(5000,2500,1250,500,100,0.5),
                labels=c(5000,2500,1250,500,100,0.5),expand=expand_scale(mult=0.005))+
  scale_y_continuous(expand=expand_scale(mult=0.005),labels = scales::percent_format(accuracy = 1))+
  geom_vline(data=z, aes(xintercept=median(res)), color="black",
             linetype="dashed", size=2)+
  xlab("Betweenness Centrality")+
  ylab("Percent of Organizations")+
  theme_classic()+
  theme(text=element_text(size=25,family="Times New Roman"),
        axis.text = element_text(size=20, color="black",family="Times New Roman"), 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

