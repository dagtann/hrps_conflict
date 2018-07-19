# figure A.3 Declining Frequencies of Mass Killing Records (5 specific variables and their sum)

library(gplots); 

data1 <- read.csv("HumanRightsProtectionScores_v2.04.csv")
data1 <- data1[order(data1$COW ), ]
data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )

# duplicate data
data.latentX <- data1

summary(data.latentX)

# prepare data to plot figure A3

data.latentX$rummel[is.na(data.latentX$rummel) ] <- 0
data.latentX$genocide[is.na(data.latentX$genocide) ] <- 0
data.latentX$massive_repression[is.na(data.latentX$massive_repression) ] <- 0
data.latentX$executions[is.na(data.latentX$executions) ] <- 0
data.latentX$killing [is.na(data.latentX$killing ) ] <- 0



data.latentX$events <-NA
data.latentX$events <- data.latentX$genocide +
  + data.latentX$rummel +
  + data.latentX$massive_repression + 
  + data.latentX$executions  +
  + data.latentX$killing 

summary(data.latentX)


#plot figure

par(new=F)
par(new=FALSE)
par(mfrow=c(3,2))

plotmeans(data.latentX$events ~ data.latentX$YEAR,  xaxt="n" , ylim=c(0.01, 0.6), 
          font.main=1, 
          barcol="white", 
          lwd=3, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="", ylab ="Annual Frequency")
axis(1, at=(1:62), label=c(1949: 2010))
legend(35 , 0.5,  pt.cex = 1 , cex=1.5 ,   bty='n',  legend = "All Events Recorded")

data.latentX$rummel[is.na(data.latentX$rummel) ] <- 0

plotmeans(data.latentX$rummel ~ data.latentX$YEAR, ylim=c(0.01, 0.31),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Annual Frequency")
legend(45 , 0.25,  pt.cex = 1 , cex=1.5 ,   bty='n',  legend = "rummel")
#text(54, 0.1  ,col="black", paste("Fariss's Estimates")) 

data.latentX$genocide[is.na(data.latentX$genocide) ] <- 0
plotmeans(data.latentX$genocide ~ data.latentX$YEAR, ylim=c(0.01, 0.31),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Annual Frequency")
legend(45 , 0.15,  pt.cex = 1 , cex=1.5 ,   bty='n', legend = "genocide")
#text(54, 0.2  ,col="black", paste("Fariss's Estimates")) 



data.latentX$massive_repression[is.na(data.latentX$massive_repression) ] <- 0

plotmeans(data.latentX$massive_repression ~ data.latentX$YEAR, ylim=c(0.01, 0.21),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Annual Frequency")
legend(40 , 0.15,  pt.cex = 1 , cex=1.5 ,   bty='n',  legend = "massive repression")
#text(54, 0.1  ,col="black", paste("Fariss's Estimates")) 

data.latentX$executions[is.na(data.latentX$executions) ] <- 0

plotmeans(data.latentX$executions ~ data.latentX$YEAR, ylim=c(0.01, 0.21),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Annual Frequency")
legend(45 , 0.15,  pt.cex = 1 , cex=1.5 ,   bty='n',  legend = "executions")
#text(54, 0.1  ,col="black", paste("Fariss's Estimates")) 

data.latentX$killing[is.na(data.latentX$killing) ] <- 0

plotmeans(data.latentX$killing ~ data.latentX$YEAR, ylim=c(0.01, 0.21),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Annual Frequency")
legend(10 , 0.15,  pt.cex = 1 , cex=1.5 ,   bty='n',  legend = "killing")
#text(54, 0.1  ,col="black", paste("Fariss's Estimates")) 

par(new=F)

par(new=FALSE)

