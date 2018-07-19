#figure 2

library(gplots); 

#load Estimates from alternative   Random Values of ALL Lesser Violations

means <- read.csv("figureA5_1_04.csv")

#load original data to combine with the estimated latent scores

data <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")

# prepare data to plot Estimates from Random Values of ALL Lesser Violations

nrow(means)
nrowmeans <- nrow(means)
rows.non.x = nrow(means) - nrow(data)
x.means <- as.matrix(means[(rows.non.x+1):nrowmeans,  ])
nrow(x.means)
all.data <- cbind(data, x.means)
summary(all.data)

#load Fariss's scores

data1 <- read.csv("HumanRightsProtectionScores_v2.04.csv")
data1 <- data1[order(data1$COW , data1$YEAR   ), ]
data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )

#plot figure 2

par(new=FALSE)
 
plotmeans(data1$latentmean  ~ data1$YEAR, ylim=c(-0.1, 1 ), 
          font.main=1, 
          barcol="white", 
          lwd=2, 
          lty =5,
          pch = "-",
          col="black", cex = 1.5,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
#legend(45, 0.2, "Fariss's Scores", bty="n" , cex=1.5);

par(new=TRUE)

plotmeans(all.data$x.means  ~ all.data$YEAR, ylim=c(-0.1, 1 ), 
          font.main=1, 
          barcol="white", 
          lwd=3,
          lty =19,
          pch = "-",
          col="blue", cex = 1.2,
          ccol="blue",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
#legend(1, 0.9, "Estimates from Random Values of ALL Lesser Violations",  bty="n" , cex=1.5);
par(lwd = 3)
legend("topleft", legend=c("Estimates from Random Values of ALL Lesser Human Rights Violations", "Fariss's Scores"),
       col=c("blue", "black"), bty="n" ,lty=1:2 , cex=1.3)

par(new=FALSE)


