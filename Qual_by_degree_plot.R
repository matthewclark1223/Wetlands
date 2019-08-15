library(extrafont)
font_import()
loadfonts(device="win")  
wets<- read.csv("~/Wetlands/GIS_Wetlands_use_for_bipartite - Sheet1.csv")
g<-asIgraph(Net)
igraph::degree(g)
x<-cbind(names(Soc[,-1]),igraph::degree(g))

wets$Condition<-as.integer(ifelse(wets$Condition=="A",4,
                                  ifelse(wets$Condition=="B", 3,
                                         ifelse(wets$Condition=="C",2,1))))
z<-wets%>%group_by(Management)%>%summarise(meanqual=mean(Condition))
x<-as.data.frame(x)
names(x)[1]<-"Organization"
names(x)[2]<-"Degree"
x<-x%>%filter(Organization ==  "BLM_Dillon"  |
             Organization ==  "Flathead_Lakers_Polson"  |
             Organization == "Flathead_Land_Trust_Kalispell"  |
             Organization =="Gallatin_Valley_Land_Trust_Bozeman" |
             Organization == "Geum_Consulting_Hamilton" |
             Organization =="Kootenai_Tribe_of_Idaho_Bonners_Ferry"   |
             Organization == "Montana_Aquatic_Resources_Services_Bozeman"  |
             Organization == "Montana_Audubon_Helena"  |
             Organization == "Morrison_Maierle_Helena" |
             Organization == "Montana_Department_of_Transportation_Wetland_Mitigation_Helena" |
             Organization =="Montana_Fish._Wildlife._and_Parks_Kalispell" |
             Organization == "Montana_Fish._Wildlife._and_Parks_Helena" |
             Organization == "Robert_Peccia_Associates_Helena" |
             Organization == "Trout_Unlimited_Missoula"  |
             Organization == "USFW.Northwest_Montana_Wetland_Management_District.Flathead_County" |
             Organization == "Watershed_Consulting_Missoula"    )

deg_qual<-cbind(x,z$meanqual)
plot(as.numeric(deg_qual$Degree), deg_qual$`z$meanqual`)
cor(as.numeric(deg_qual$Degree), deg_qual$`z$meanqual`)
ggplot(deg_qual, aes(x=as.numeric(Degree), y=`z$meanqual`))+geom_point(size=6)+geom_smooth(method="lm", se=F, color="black", size=2)+
  theme_few() +xlab("Organization Collaborations")+
  ylab("Average Reported Wetland Quality")+
  theme(text=element_text(family="Times New Roman",size=30),legend.position="none",
        axis.text = element_text(size=25, color="black"),axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20)))+
  annotate(x=7, y=2.5, 
           label=paste("Cor = ", round(cor(as.numeric(deg_qual$Degree), deg_qual$`z$meanqual`),2)), 
           geom="text", size=15, family="Times New Roman")

