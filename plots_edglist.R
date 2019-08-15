links<- get.data.frame(g)
nodes<-as.data.frame(names(SenMat))
names(nodes)[1]<-"id"

visNetwork(nodes, links, width="100%", height="400px")
g=graph_from_adjacency_matrix(m,mode="directed",weighted=NULL) 
links<- get.data.frame(g)
q<-links[links$to == "USFW-Northwest_Montana_Wetland_Management_District-Flathead_County",]
nrow(q)
View(q)
View(links)
