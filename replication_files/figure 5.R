#load Fariss's scores

data1 <- read.csv("HumanRightsProtectionScores_v2.04.csv")
data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )
summary(data1)

#load latent score calculated allowing All Intercepts Vary
#The scores are calculated with all indicators assumed to have 
#a similar relationship to the latent variable, i.e., all intercepts are allowed to vary. 
#The assumption is that there also has been a change in the standards for recording mass killings.

data5 <- read.csv("figure5_04.csv")
summary(data5)
data5$latentX <- data5$V14

#plot figure 5

par(new=FALSE)
plotmeans(data1$latentmean  ~ data1$YEAR, ylim=c(-0.2,1),  
          font.main=1, 
          barcol="white", 
          pch = "o", lwd=2,
          col="black", cex = 1.3,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Latent scores", 
          cex.lab=1.8, cex.axis=1.8)

text(52, 0.9 ,  cex=1.9 ,col="black", paste("Fariss's Estimates")) 
par(new=TRUE)

plotmeans(data5$latentX  ~ data5$YEAR, ylim=c(-0.2,1),    
          font.main=1, 
          barcol="white", 
          lwd=4,
          pch = 16,
          col="darkblue", cex = 1,
          ccol="darkblue",
          n.label="F", 
          xlab ="Year", ylab ="Latent scores", 
          cex.lab=1.8, cex.axis=1.8)

text(52, 0 ,  cex=1.9  ,col="black", paste("All Intercepts Vary")) 

par(new=FALSE)

