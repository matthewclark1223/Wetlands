install.packages("statnet", dependencies = TRUE) 
num_nodes <- 10
library(statnet)
my_sociomatrix <- matrix(round(runif(num_nodes*num_nodes)), # edge values
                         nrow = num_nodes, #nrow must be same as ncol
                         ncol = num_nodes)
net <- as.network(x = my_sociomatrix, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

network.vertex.names(net) <- LETTERS[1:10]

network.vertex.names(net) <- c("Susan","Rachel","Angela","Carly","Stephanie","Tom","Mike","Tony","Matt","Steven")

# Create the variable
gender <- c(rep("Female",num_nodes/2),rep("Male",num_nodes/2))
# Take a look at our variable
print(gender)
# Add it to the network object
set.vertex.attribute(net, # the name of the network object
                     "Gender", # the name we want to reference the variable by in that object
                     gender # the value we are giving that variable
) 

age <- round(rnorm(num_nodes,20,3))
set.vertex.attribute(net,"Age",age)

summary.network(net, # the network we want to look at
                print.adj = TRUE # if TRUE then this will print out the whole adjacency matrix.
)

node_colors <- rep("",num_nodes)
for(i in 1:num_nodes){
  if(get.node.attr(net,"Gender")[i] == "Female"){
    node_colors[i] <- "lightblue"
  }else{
    node_colors[i] <- "maroon"
  }
}
print(node_colors)

plot.network(net, # our network object
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)

