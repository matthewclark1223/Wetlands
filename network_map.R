devtools::install_github("dkahle/ggmap")
library(ggmap)
library(maps)
library(ggnetwork)
library(geomnet)
library(GGally)
invisible(lapply(c("ggplot2", "maps", "network", "sna"), base::library, character.only = TRUE))
register_google(key = "AIzaSyAs88ik0Q7x8yY7JcvRzVLSYvUt9IkQr58")
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



ggnetworkmap(mon, Net,node.group = Type ,size=20,alpha = 0.7, weight = degree)+
  scale_color_manual(values=c("#8dd3c7","#fb8072","#bebada","#b3de69","#80b1d3","#fdb462"))
  


#network map with wetlands
wets<- read.csv("~/Wetlands/GIS_Wetlands_2 - Sheet1.csv")
y<-as.network(finalmat, directed = F)
wetty<-rep("Wet",55)
socy<-rep("Social",169)
grouping<-c(socy,wetty)
set.vertex.attribute(y, "Type", as.character(grouping))
lat<-c(as.character(Nodes$Lat), as.character(wets$Lat))
lon<-c(as.character(Nodes$Long), as.character(wets$Long))
set.vertex.attribute(y, "lat", as.character(lat))
set.vertex.attribute(y, "lon", as.character(lon))

colorz<-c("Social"="forestgreen","Wet"="steelblue")
ggnetworkmap(mon, y, node.group = Type , size=10,alpha = 0.75)+scale_color_manual(values=c("#1f78b4","#b2df8a"))


#Just wetland map
wets<- read.csv("~/Wetlands/GIS_Wetlands_2 - Sheet1.csv")
wetcon5<-read.csv("~/Wetlands/Wetland Connectivity/Adj_mat_20KM.csv")
wetcon52 <- wetcon5[,-1]
rownames(wetcon52) <- wetcon5[,1]
wetcon5<-as.matrix(wetcon52)
wetcon5<-as.network(x=wetcon5, directed=F)
set.vertex.attribute(wetcon5, "lat", as.character(wets$Lat))
set.vertex.attribute(wetcon5, "lon", as.character(wets$Long))
mon<-ggplot(mt, aes(x=long, y=lat))+
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

#plot

ggnetworkmap(mon, wetcon5, node.color = "#33a02c", segment.color ="#33a02c", segment.size = 2,size=5,alpha = 0.5 )+
  theme(
    panel.background = element_rect(fill = "#073763ff",
                                    colour = "#073763ff"))+ 
  coord_quickmap(xlim = c(-116, -113.9),  ylim = c(48, 49))
#

ggnetworkmap(mon, wetcon5, node.color = "#33a02c", segment.color ="#33a02c", segment.size = 2,size=8,alpha = 0.5 )+
  theme(
    panel.background = element_rect(fill = "#073763ff",
                                    colour = "#073763ff"))+ 
  coord_quickmap(xlim = c(-115.5, -113.9),  ylim = c(48, 48.66))

layout<-igraph::layout_nicely(asIgraph(wetcon5))
layout<-igraph::layout_with_fr(asIgraph(wetcon5))
plot(asIgraph(wetcon5), layout=layout)







counties <- map_data("county")
MT_county <- counties %>%
  filter(region == "montana")
states <- map_data("state")
MT_df <- states %>%
  filter(region == "montana")


MT_base <- ggplot(data = MT_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "gray")
MT_base + theme_void()+geom_polygon(data = MT_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)
