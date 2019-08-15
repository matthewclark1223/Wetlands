library(readr)
library(ggplot2)
library(dplyr)
#Fully Fixed Wetland network
occ <- read_csv("~/Wetlands/Structure Occurrences --varying connectivities  - Sheet1 (1).csv")
occ1<-occ%>%filter(Shape == "C4AXB"|Shape == "L3XAX")
occ2<-occ%>%filter(Shape == "L3AXB")
ggplot()+geom_line(data=occ1,aes(x=Expected, y=Observed, color=Shape), size=5)+
  geom_line(data=occ2,aes(x=Expected/100, y=Observed/100, color=Shape), size=5)+theme_classic()
ggplot(occ,aes(x=Expected, y=Observed, color=Shape))+geom_smooth()+theme_classic()

#Fixed density, Wetland Network
occ <- read_csv("~/Wetlands/Structure Occurrences_Fix_dense_wets - Sheet1.csv")
occ1<-occ%>%filter(Shape == "C4AXB")
occ2<-occ%>%filter(Shape == "Star2AX")
occ3<-occ%>%filter(Shape == "TXAX")
ggplot()+geom_line(data=occ3,aes(x=Expected*100, y=Observed, color=Shape), size=5)+
  geom_point(data=occ3,aes(x=Expected*100, y=Observed, color=Shape), size=20)+
  geom_line(data=occ2,aes(x=Expected, y=Observed, color=Shape), size=5)+
  geom_point(data=occ2,aes(x=Expected, y=Observed, color=Shape), size=20)+
  scale_color_manual(values=c("#1f78b4", "#b2df8a"))+
  theme_classic()+
  ylim(0,35)+
  xlab("Expected Count")+
  ylab("Observed Count")+
  ggtitle("Expected vs Observed Shape Counts:\nVarying Connectivity Thresholds")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))
  


