devtools::install_github("dkahle/ggmap")
library(ggmap)
library(maps)
library(ggnetwork)
library(geomnet)
library(GGally)
invisible(lapply(c("ggplot2", "maps", "network", "sna"), base::library, character.only = TRUE))
register_google(key = "XXXXXX")
mt<-map_data(database = "state", "montana")

Net<-as.network(x=Net, directed=F, loops=F, matrix.type="adjacency")
#Set vertx attributes
set.vertex.attribute(Net, "Type", as.character(Nodes$Type))
set.vertex.attribute(Net, "lat", as.character(Nodes$Lat))
set.vertex.attribute(Net, "lon", as.character(Nodes$Long))
Net %v% "degree" <- degree(Net, gmode = "digraph")
mon<-ggplot(mt, aes(x=long, y=lat))+
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

ggnetworkmap(mon, Net,node.group = Type ,size=20,alpha = 0.25, weight = degree)
                                                                       

y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 5, 6, 8, 2) ]
names(y)=levels(Nodes$Type)
ggnet2(Net, color="Type",palette = y, alpha=0.75, size=4, edge.alpha=0.5,
       edge.color = c("color", "grey50"))

ggnetworkmap(mon, Net,node.group = Type ,palette=y,edge.alpha=0.5,size=20,alpha = 0.5, 
             edge.color = c(node.group, "grey50"),weight = degree)
