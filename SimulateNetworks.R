generate.network = function(n, p) {
  
  # Generate matrix values, sampling 0 or 1 with given probabilities
  matvals = sample(c(0, 1), n * (n - 1)/2, replace = TRUE, prob = c(1 - p,
                                                                    p))
  
  # From the values above, generate a symmetric matrix
  networkmat = matrix(rep(0, n * n), ncol = n)
  mv = 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        networkmat[i, j] = matvals[mv]
        networkmat[j, i] = matvals[mv]
        mv = mv + 1
      }
    }
  }
  return(networkmat)
}

#test it
(network <- generate.network(200, 0.8))


#bring in statnet


net <- as.network(x = network, # the network object
                  directed = F, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

network.vertex.names(net) <- paste0("S",1:10)


summary.network(net, # the network we want to look at
                print.adj = TRUE # if TRUE then this will print out the whole adjacency matrix.
)
plot(net)








