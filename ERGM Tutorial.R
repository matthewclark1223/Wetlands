library(statnet)
data('sampson')
n = samplike
list.network.attributes(n) ##get attributes for the entire network
?attribute.methods ##Info on the different ways to set/examine/delete attrivutes of the network, 
#vertecies, and edges.

n %v% 'group' # This reads as: get 'v'ertex attributes for 'group' from object 'n'

n %v% 'cloisterville' #Which 'monks'v'ertecies have the attribute for attending 'cloisterville'
## as above, %e% and %n% can be used for assignment and retrieval of 'e'dge and 'n'etwork attributes

#Estimate the simplest model, one with only a term for tie density (akin to an intercept term in a glm):
m1 = ergm(n ~ edges)
summary(m1)

#The edges output represents exactly the density of the ties in out network
# check that here
all.equal(network.density(n), plogis(coef(m1)[[1]]))

##Now let’s make things more interesting and estimate a term for reciprocity of ties. 
#That is, given an i -> j tie, what is the change in log-odds likelihood of a j -> i tie? 
#The coefficient estimate on 'mutual' tells us exactly that:

m2 = ergm(n ~ edges + mutual)
summary(m2)

#Now interpret these coefficients 
#this is the baseline probability of a tie
plogis(coef(m2)[['edges']])

##But if you look at the 'mutual' estimate, we can see that the log odds of a tie is 2.3
## times greater if the reciprocal tie is present

## translate this into a probability so we can interpret it
plogis(coef(m2)[['edges']] + coef(m2)[['mutual']])


# we can see now that there is a 64 chance of a tie if the reciprocal tie is present
## compare this to the baseline of 15% <- rember we got this from plogis(coef(m2)[['edges']])

##Before we start writing up our submission to Science though, 
##we need to check two things: 
##1) that the MCMC routine behaved well (that our estimates are likely good approximations of the MLEs), 
#and 2) that our model fits the data well. statnet has functions to do both those things.

mcmc.diagnostics(m2)

##this ^^ checks our model estimates, now lets see if they fit our data
## gof simulates networks from the ERGM estimates and, for some set of network statistics, 
#compares the distribution in the simulated networks to the observed values.

#this will simulate networks based on our network statistics and compare them to our 
#observed network. For this, the higher the p-value the better

m2_gof = gof(m2, GOF = ~model)
plot(m2_gof)

#Let's plot some other gof stats
#black lines represent the OBSERVED distributions, Boxplots represtne SIMULATED networks
m2_gof2 = gof(m2)
par(mfrow = c(2,2))
plot(m2_gof2)

##To change which statistics are included, specify them as a model formula 
##to the GOF argument to the gof function.
#E.g. gof(m2, GOF = ~ triadcensus + odegree + idegree). 
#The list of supported statistics is available in the help file for gof.

#We can also Simulate four networks that are consistent with our model 
#and plot them, as we plotted the observed network above:

sim_nets = simulate(m2, nsim = 4)

# Define a plotting function:
plot_nets = function(n)
  plot(n
       , displaylabels = FALSE
       , vertex.cex = degree(n, cmode = 'indegree') / 2 + 1
       , vertex.col = 'group'
       , vertex.sides = ifelse(n %v% 'cloisterville', 4, 50)
  )

par(mfrow = c(2, 2))
invisible(lapply(sim_nets, plot_nets))

?statnet































