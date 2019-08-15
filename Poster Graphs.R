ExpectedL3XAX<-rnorm(1000,4.143, 3.441)
ExpectedL3XBX<-rnorm(1000, 793.1660, 26.735)
ExpectedL3XAB<-rnorm(1000, 689.9420, 243.672)
ExpectedC4AXB<-rnorm(1000,  2.0940, 2.033)


h1<-hist(ExpectedL3XAX,plot=FALSE)


h2<-hist(ExpectedL3XBX,plot=FALSE )


h3<-hist(ExpectedL3XAB,plot=FALSE)

h4<-hist(ExpectedC4AXB,plot=FALSE)



plot(h1, main="Management of Connected Resources",cex.lab=2, cex.main=2,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,xlim=c(0, 17),col="deepskyblue")
abline(v = 4,
       col = "red",
       lwd = 3)
box()


plot(h2, main="Collaborative Management of Connected Resources",cex.lab=1.5, cex.main=1.5,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,xlim=c(550, 900),col="deepskyblue")
abline(v = 567,
       col = "red",
       lwd = 3)
box()



plot(h3, main="Collaborative Management of Connected Resources",cex.lab=1.5, cex.main=1.5,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,col="deepskyblue")
abline(v = 269,
       col = "red",
       lwd = 3)
box()




plot(h4, main="Collaborative Management of Connected Resources",cex.lab=1.5, cex.main=1.5,
     xlab="Expected Count",ylab="Frequency",cex.axis = 1.5,col="deepskyblue")
abline(v = 1,
       col = "red",
       lwd = 3)
box()



ExpectedL3XAX<-rnorm(1000,  78.1670, 22.698	)

x<-as.data.frame(ExpectedL3XAX)

ggplot(data=x,aes(x=ExpectedL3XAX))+geom_density(fill="deepskyblue")+
  theme_classic()+#xlim(0,10)+ 
  geom_vline(xintercept=80, color="red", lwd=2) +
  xlab("Substructure Count")+
  ylab("Density of Random Samples")+
  ggtitle("Expected vs Observed Shape Counts:")+
  theme(text=element_text(size=30),#legend.position="none",
        axis.text = element_text(size=25, color="black"),plot.title = element_text(lineheight=.8, face="bold"))
