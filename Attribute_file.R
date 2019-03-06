library(readr)
library(dplyr)
soc_mat<-read_csv("~/Wetlands/Social_net.csv")
all_wetlands<-read_csv("~/Wetlands/GIS_Wetlands_2 - Sheet1.csv")
all_wetlands<-na.omit(all_wetlands)
attrib<-all_wetlands%>%select(Condition)
  
attrib<-as.data.frame(ifelse(attrib$Condition == "A", 4,
         ifelse(attrib$Condition == "B", 3,
                ifelse(attrib$Condition =="C",2,1))))
names(attrib)[1]<-"Condition"

soc_attrib<-data.frame(matrix(0,nrow=nrow(soc_mat),ncol=1))
names(soc_attrib)[1]<-"Condition"
attrib<-rbind(soc_attrib,attrib)
write.csv(attrib, file="Attribute_file.csv")
