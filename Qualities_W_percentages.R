library(readr)
library(dplyr)
library(ggplot2)
data<- read_csv("~/Wetlands/Structures with 1's - Sheet1.csv")
x<-data%>% group_by(Structure)%>%mutate(pct=(Total_high/Wetland_count)*100)
x<-data%>% group_by(Structure)%>%mutate(pct=(Subtracted_Qual_tot/Subtracted_wet_tot)*100)

x20<-x[x$Distance==20,]
x10<-x[x$Distance==10,]
x5<-x[x$Distance==5,]
x1<-x[x$Distance==1,]

x<-x[x$Distance==2|x$Distance==5|x$Distance==10|x$Distance==20,]

x$Structure<-as.character(ifelse(x$Structure=="C4AXB", "Collaborative", "Independant"))

#plot for presentation slide
ggplot(x, aes(x=Structure, y=pct))+
  geom_point(aes(color=Structure),size=35)+
  geom_segment(aes(color=Structure, y=20, yend=pct, x=Structure, xend=Structure), size=5)+
  scale_color_manual(values=c("#1f78b4", "#b2df8a"),labels=c("Collaboration", "No Collaboration"))+
  ylim(20,100)+theme_few()+ facet_grid(.~Distance)+xlab("Connectivity Threshold")+
  ylab("Percent of wetlands at or Near Reference Condition")+
  ggtitle("Change in Percent of High Quality Wetlands with\nVarying Connectivity Thresholds")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),strip.text.x = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold"),axis.title.x = element_text(margin = margin(t = 150)))


#plot for paper 
ggplot(x, aes(x=Structure, y=pct))+
  geom_point(aes(color=Structure),size=40)+
  geom_segment(aes(color=Structure, y=20, yend=pct, x=Structure, xend=Structure), size=5, show.legend = F)+
  scale_fill_manual(values=c("#737373", "#d9d9d9"),labels=c("Collaboration", "No Collaboration"))+
  scale_color_manual(values=c("#737373", "#d9d9d9"),labels=c("Collaboration", "No Collaboration"))+
 theme_few()+ facet_grid(.~Distance)+xlab("Connectivity Threshold")+
   scale_y_continuous(labels=function(x) paste0(x,"%"), limits=c(20,90),expand=expand_scale(mult=0.005))+
  ylab("Wetlands at or Near Reference Condition")+
  theme(text=element_text(size=30,family="Times New Roman"),#legend.position="none",
        axis.text = element_text(size=25, color="black",family="Times New Roman"),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),strip.text.x = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold",family="Times New Roman"),
        axis.title.x = element_text(margin = margin(t = 150)))



