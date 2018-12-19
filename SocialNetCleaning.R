library(readr)
data<-read_csv("~/Wetlands/Full_Survey_Results (2).csv")
View(data)
#remove the response from one of the researchers
#data<-data[data$RecipientEmail!="matthewclark989@boisestate.edu",]....Why doesn't this work????...whatever...
data<-data[-65,] #CSK tribe double response, same IP address
data<-data[-40,] #MT DOT survey pretest response
data<-data[-27,] #Respondant identified as "environmental"
data<-data[-6,] #Matt Clark response

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
SenMat[,1]<-gsub(" ", "_",SenMat[,1])


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


for(i in 1:124 ){
  SenMat<-rbind(rep(0,164),SenMat)
}

SenMat$BLM_Glasgow_MT<-rep(0,164)
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
row.names(SenMat)[which(row.names(SenMat)=="Five_Valleys_Land_Trust,Missoula,_MT")]= "Five_Valleys_Land_Trust_Missoula"
row.names(SenMat)[which(row.names(SenMat)=="Flathead_Lakers,Polson,_MT")]= "Flathead_Lakers_Polson"
row.names(SenMat)[which(row.names(SenMat)=="Flathead_Land_Trust,Kalispell,_MT")]= "Flathead_Land_Trust_Kalispell"
row.names(SenMat)[which(row.names(SenMat)=="Gallatin_Cons._district,Manhatten,_MT")]= "conservation_district_Gallatin"
row.names(SenMat)[which(row.names(SenMat)=="Gallatin_Valley_Land_Trust,Bozeman,_Montana")]= "Gallatin_Valley_Land_Trust_Bozeman"
row.names(SenMat)[which(row.names(SenMat)=="Geum_Environmental_Consulting,_Inc.,Hamilton,_Montana")]= "Geum_Consulting_Hamilton"
row.names(SenMat)[which(row.names(SenMat)=="Kaniksu_Land_Trust,Sandpoint,_ID")]= "Kaniksu_Land_Trust_Sandpoint"
row.names(SenMat)[which(row.names(SenMat)=="Lower_Musselshell_Conservation_District,Roundup,_MT")]= "conservation_district_Lower_Musselshell"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Aquatic_Resources_Services_(MARS),Office:_Livingston,_MT;_Work:_Statewide")]= "Montana_Aquatic_Resources_Services_Bozeman"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Audubon,Helena_MT")]= "Montana_Audubon_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Department_of_Transportation,Helena,_MT")]= "Montana_Department_of_Transportation_Wetland_Mitigation_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Montana_DEQ,Helena,_MT")]= "Montana_Department_of_Environmental_Quality_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Fish,_Wildlife_&_Parks,Kalispell,_MT")]= "Montana_Fish,_Wildlife,_and_Parks_Kalispell"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Fish,_Wildlife_and_Parks,Helena")]= "Montana_Fish,_Wildlife,_and_Parks_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Watershed_Coordination_Council,Helena,_MT")]= "Montana_Watershed_Coordination_Council_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Northern_Cheyenne_Tribe,Lame_Deer,_MT")]= "Northern_Cheyenne_Tribe_Lame_deer"
row.names(SenMat)[which(row.names(SenMat)=="Montana_Conservation_Corps,Bozeman")]= "Big_Sky_Watershed_Corps"
row.names(SenMat)[which(row.names(SenMat)=="River_Design_Group,_Inc.,Whitefish,_MT")]= "River_Design_Group_Whitefish"
row.names(SenMat)[which(row.names(SenMat)=="The_Nature_Conservancy,Montana")]= "The_Nature_Conservancy_Bozeman"
row.names(SenMat)[which(row.names(SenMat)=="The_Nature_Conservancy,Western_Montana")]= "The_Nature_Conservancy_Missoula"
row.names(SenMat)[which(row.names(SenMat)=="Trout_Unlimited,Missoula")]= "Trout_Unlimited_Missoula"
row.names(SenMat)[which(row.names(SenMat)=="U.S._Army_Corps_of_Engineers,Missoula,_MT")]= "Army_Corps_of_Engineers_Missoula"
row.names(SenMat)[which(row.names(SenMat)=="Upper_Musselshell_Conservation_District,Harlowton,_MT")]= "conservation_district_Upper_Musselshell"
row.names(SenMat)[which(row.names(SenMat)=="US_Army_Corps_of_Engineers,Helena,_Montana")]= "Army_Corps_of_Engineers_Helena"
row.names(SenMat)[which(row.names(SenMat)=="USFWS,Blackfoot_Valley_Montana")]= "USFW-Benton_Lake_Wetland_Management_District"
row.names(SenMat)[which(row.names(SenMat)=="USFWS,Lost_Trail_NWR")]= "USFW-Northwest_Montana_Wetland_Management_District-Flathead_County"
row.names(SenMat)[which(row.names(SenMat)=="USGS,Helena,_Mt")]= "USGS-Wyoming-Montana_Water_Science_Center_Helena"
row.names(SenMat)[which(row.names(SenMat)=="Watershed_Consulting_LLC,Missoula,_Montana")]= "Watershed_Consulting_Missoula"
row.names(SenMat)[which(row.names(SenMat)=="Beaverhead_Watershed_Committee,Dillon,_MT")]= "conservation_district_Beaverhead"
row.names(SenMat)[which(row.names(SenMat)=="BLM,Glasgow,_MT")]= "BLM_Glasgow_MT"




#Use this to deal with all the numbered row names
x<-row.names(SenMat)
y<-names(SenMat)
NewN<-which(y %in% x == FALSE)
numbered<-which(x %in% y == FALSE)

rownames(SenMat)[numbered]<-names(SenMat)[NewN]


#Check to make sure the row names and column names are the exact same. 
which(row.names(SenMat) %in% names(SenMat) ==FALSE)

#Make the df numeric
for(i in 1:ncol(SenMat)){
  SenMat[,i]<-as.numeric(SenMat[,i])
}
#Order rows and columns
SenMat <- SenMat[ order(row.names(SenMat)), order(names(SenMat))]
#Make an igraph object
library(igraph)

#Let's make it binary
SenMat[SenMat==0]<-0
SenMat[SenMat==1]<-1
SenMat[SenMat==2]<-1
SenMat[SenMat==3]<-1
SenMat[SenMat==4]<-1
SenMat[SenMat==5]<-1
###

m=as.matrix(SenMat)
g=graph_from_adjacency_matrix(m,mode="undirected",weighted=NULL)

plot(g, vertex.size=6,vertex.label=NA)
plot(g, vertex.size=6,vertex.label=NA, rescale=F,ylim=c(-3,5),xlim=c(-5,5), asp = 0)

edge_density(g,loops=F)
diameter(g,directed=F, weights=NA)

deg<-degree(g, mode="all")
plot(g,vertex.size=deg*0.2,vertex.label=NA)
hist(deg,breaks=1:vcount(g)-1,main="Hist node degree")

