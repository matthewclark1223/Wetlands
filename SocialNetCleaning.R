library(readr)
data<-read_csv("~/Wetlands/Full_Survey_Results (2).csv")
View(data)
#remove the response from one of the researchers
#data<-data[data$RecipientEmail!="matthewclark989@boisestate.edu",]....Why doesn't this work????...whatever...
data<-data[-40,]
data<-data[-6,]

#Remove irrelevant columns
data<-data[,-c(1:17)]
data<-data[-2,]

#Remove Wetlands data
data<-data[,-c(356:441)]

#remove organizations that do not participate in wetland management
data<-data[data$Q2 != "No" ,]


#We can also get rid of columns 3 & 4
data<-data[,-c(4:5)]

data[1,]<-gsub("Please indicate to what degree ", "",data[1,])
data[1,]<-gsub("your organization shares information and/or cost shares ", "",data[1,])
data[1,]<-gsub("with each... - How often does your organization share ", "",data[1,])
data[1,]<-gsub("with this organization regarding wetland ", "",data[1,])
data[1,]<-gsub("management/conservation?", "",data[1,])
data[1,]<-gsub("United States Forest Service", "USFS",data[1,])
data[1,]<-gsub("United States Fish & Wildlife Service", "USFW",data[1,])
data[1,]<-gsub("On how many projects in the last two years has your organization", "",data[1,])
data[1,]<-gsub("with this organization", "",data[1,])
data[1,]<-gsub("with each", "",data[1,])
data[1,]<-gsub("United States Geological Survey", "USGS",data[1,])
data[1,]<-gsub("Bureau of Land Management", "BLM",data[1,])
data[1,]<-gsub("Environmental Protection Agency", "EPA",data[1,])
data[1,]<-gsub("On how many wetland projects in the last two years has your organization", "",data[1,])
data[1,]<-gsub("with this ", "",data[1,])
data[1,]<-gsub("regarding wetland ", "",data[1,])
data[1,]<-gsub("(", "",data[1,],fixed=T)
data[1,]<-gsub(")", "",data[1,])


names(data)[1]<-"Q1"

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

data<-completeFun(data, c("Q1","Q2"))

#Next, make it only information sharing ties

data<-data[,-c(3:4)]

x<-data[,grep('#1',names(data))]
SenMat<-cbind(data[,c(1:2)],x)
View(SenMat)
SenMat[1,]<-gsub("information", "",SenMat[1,])
SenMat[1,]<-gsub(" - ", "",SenMat[1,])
SenMat[1,]<-gsub("?", "",SenMat[1,], fixed=TRUE)
SenMat[1,]<-gsub(" ", "_",SenMat[1,])
SenMat[,1]<-gsub(" ", "_",SenMat[1,])


colnames(SenMat) = SenMat[1, ] # the first row will be the header
SenMat = SenMat[-1, ] 

SenMat[,1]<-paste(SenMat[,1],SenMat[,2], sep = ",")
SenMat=SenMat[,-2]
names(SenMat)[1]<-"Organization"

#Now let's replace all the na's with 0 and remove the text from inside the matrix
SenMat[is.na(SenMat)] <- 0

SenMat[SenMat=="0 - Never share information"]<-0
SenMat[SenMat=="0-Never share information"]<-0
SenMat[SenMat=="1 - At least once a year"]<-1
SenMat[SenMat=="2 - At least once every 6 months"]<-2
SenMat[SenMat=="2 - At least once every six months"]<-2
SenMat[SenMat=="3 - At least once a month"]<-3
SenMat[SenMat=="4 - Once a week or more"]<-4
SenMat[SenMat=="5 - Daily"]<-5

#Now we need to make the columns and rows match
#Let's aplhabatie them first

SenMat2 <- SenMat[,-1]
rownames(SenMat2) <- SenMat[,1]
SenMat<-SenMat2

SenMat<-SenMat[ , order(names(SenMat))]
SenMat<-SenMat[ order(row.names(SenMat)), ]

SenMat<-SenMat[,-c(164:173)]

names(SenMat)<-substring(names(SenMat),2) #remove hanging underscore

#Now create rows to match all of the columns

for(i in 1:121 ){
  SenMat<-rbind(rep(0,163),SenMat)
}

#Now make the "old" names match the newer ones

row.names(SenMat)=gsub(" ","_",row.names(SenMat)) #Get rid of any spaces

#Make all the names match from the 42 we have data for
row.names(SenMat)[which(row.names(SenMat)=="Big_Hole_Watershed_Committee,Big_Hole_River_Watershed")]= "Big_Hole_Watershed_Committee_Divide" 
row.names(SenMat)[which(row.names(SenMat)=="Blackfeet_Environmental_office,Browning_Mt")]= "Blackfeet_Nation_Blackfeet_Reservation"
row.names(SenMat)[which(row.names(SenMat)=="BLM,Butte,_MT")]= "BLM_Butte"
row.names(SenMat)[which(row.names(SenMat)=="BLM,Miles_City,_MT")]= "BLM_Miles_City"
row.names(SenMat)[which(row.names(SenMat)=="Bureau_of_Land_Management,Dillon,_MT")]= "BLM_Dillon"
row.names(SenMat)[which(row.names(SenMat)=="Cascade_Conservation_District,Great_Falls,_MT")]= "conservation_district_Cascade"
row.names(SenMat)[which(row.names(SenMat)=="Chippewa_Cree_Tribe,Rocky_Boy's_Indian_Reservation")]= "Rocky_Boy_Reservation"
row.names(SenMat)[which(row.names(SenMat)=="Confederated_Salish_Kootenai_Tribes,Polson,_MT")]= "Confederated_Salish_and_Kootenai_Tribes’_Fisheries_Program_Pablo"
row.names(SenMat)[which(row.names(SenMat)=="DOWL,Billings")]= "Dowl_Billings"
row.names(SenMat)[which(row.names(SenMat)=="Du,Bozeman,_mt")]= "Ducks_Unlimited_Bozeman"






SenMat$duc


#Use this to deal with all the numbered row names
rownames(SenMat)[1:2]<-c("Army_Corps_of_Engineers_Billings","Army_Corps_of_Engineers_Helena")

