library(readr)
library(statnet)
library(dplyr)
library(ggplot2)
library(ggnetwork)
wetlandsOnly<-read_csv("~/Wetlands/Wetlands for GIS - Sheet1 (1).csv")
wetnet<-read_csv("~/Wetlands/Prelim SEN Matrix - Sheet1 (2).csv", col_names = FALSE)
NodeList<-read_csv("~/Wetlands/SENA Node List - Sheet1.csv")
?read_csv
wetnet<-as.matrix(wetnet, dimnames=NULL)
net <- as.network(x = m, # the network object
                  directed = FALSE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)


View(wetnet)

names(wetnet)<-NULL
el=read.csv("~/Wetlands/Prelim SEN Matrix - Sheet1 (2).csv",header=TRUE,row.names=1,check.names=FALSE) # choose an adjacency matrix from a .csv file
el=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) # choose an adjacency matrix from a .csv file
m=as.matrix(el) # This coerces the object into a matrix, just in case
net=network(m,matrix.type="adjacency",directed=FALSE) # This converts the matrix into a an undirected "network object"
net=network(m)
View(as.matrix(net))

wetnet=read.csv("~/Wetlands/Prelim SEN Matrix - Sheet1 (2).csv",header=TRUE,row.names=1,check.names=FALSE) # choose an adjacency matrix from a .csv file
m=as.matrix(wetnet)
net=network(m,matrix.type="adjacency",directed=FALSE) # This converts the matrix into a an undirected "network object"

View(as.matrix(net))
net
View(wetnet)
View(m)
?network.adjacency

######################
#Specifying Colors 

AHS_NodeList <- NodeList %>% 
  mutate (Color_Condition = ifelse(Condition == "Social", 'red', ifelse(Condition == "A", 'green', 
                                                         ifelse(Condition == "B", 'green3', 
                                                                ifelse(Condition == "C", 'green4', 'seagreen4')))))

#Creating Vectors to Assign as Attributes to the Network
Condition <- as.vector(AHS_NodeList$Condition)  
Color_Condition <- as.vector(AHS_NodeList$Color_Condition)          #Important: 2d network Plots require a vector for an attribute

#Assigning Attributes to Vertices
set.vertex.attribute(net,"Condition",Condition)
set.vertex.attribute(net,"Condition",Color_Condition)





summary(net)
par(mar=c(10,10,10,10))
    
    
ggnetwork(net) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(color = Color_Condition, size=4) +       
  theme_blank() + 
  geom_density_2d()
m
g<-plot.network.default(net, # our network object
vertex.col = Color_Condition,
displayisolates = T,
edge.col = "lightgray",         
vertex.cex = 70*(colSums(as.matrix(net)) + 1)/sum(as.matrix(net)),
displaylabels = T # show the node names
  # display the names directly over nodes
                       )


library(xergm)
?tnam
?ergmterms
View(as.matrix(net))


#********************************
wetnet=read.csv("~/Wetlands/Prelim SEN Matrix - Sheet1 (2).csv", header=T,row.names=1,check.names=F) # choose an adjacency matrix from a .csv file
m=as.matrix(wetnet)
net=as.network.default(m,matrix.type="adjacency",directed=F) # This converts the matrix into a an undirected "network object"
View(m)
View(as.matrix(net))

#**********************************




