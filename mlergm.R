install.packages("mlergm")
library(mlergm)
library(readr)
library(network)

#example 
# Estimate a curved multilevel ergm model with offset parameter
# Approximate run time (2 cores): 1.2m, Run time (3 cores): 55s
data(classes)
model_est <- mlergm(classes ~ edges + mutual + nodematch("sex") + gwesp(fixed = FALSE),
                    seed = 123,
                    options = set_options(number_cores = 3))
# To access a summary of the fitted model, call the 'summary' function
summary(model_est)
# Goodness-of-fit can be run by calling the 'gof.mlergm' method
# Approximate run time (2 cores): 48s, Run time (3 cores): 34s
gof_res <- gof(model_est, options = set_options(number_cores = 2))
plot(gof_res, cutoff = 15)


net <- simulate_mlnet(form = network.initialize(40, directed = FALSE) ~ edges + gwesp,
                      node_memb = c(rep(1, 20), rep(2, 20)),
                      theta = c(-3, 0.5, 1.0),
                      between_prob = 0.01,
                      options = set_options(number_cores = 2, burnin = 5000))


#Make all networks into one network
Bipartite<- read.csv("~/Wetlands/bipartite_network_as.df.csv")
Eco<-read.csv("~/Wetlands/Wetland Connectivity/Adj_mat_2KM.csv")
Soc<-read.csv("~/Wetlands/Social_net.csv")

Mixed_1<-cbind(Bipartite,Eco)
Mixed_2<-rbind(Soc,Bipartite)

int_1<-Mixed_1[,172:226]
int_2<-data.frame(matrix(nrow=169, ncol=55))
names(int_2)<-names(int_1)
int_3<-rbind(int_2,int_1)
final<-cbind(Mixed_2,int_3)
final[is.na(final)]<-0
final2 <- final[,-1]
rownames(final2) <- final[,1]
finalmat<-as.matrix(final2)
#Make it an mlnet object
wetty<-rep("Wet",55)
socy<-rep("Social",169)
grouping<-c(socy,wetty)
y<-as.network(finalmat, directed = F)
x<-mlnet(y,node_memb = grouping)

#Try gof
model_est <-mlergm(x ~ edges + mutual, 
                          seed = 123, 
                          parameterization = "offset", options=set_options(number_cores = 4))
summary(model_est)
gof_res <- gof(model_est, options = set_options(number_cores = 2))
plot(gof_res, cutoff = 15, pretty_x = T)

Net<-mlnet(Net,node_memb = "Type")
model_est <-mlergm(Net ~ edges + mutual, 
                   seed = 123, 
                   parameterization = "offset", options=set_options(number_cores = 4))
summary(model_est)





