
#figure 3 Figure 3: A Comparison of Latent Human Rights Trends Estimated Using 
# only Indicators of Lesser Human Rights Violations 
# or only Mass Killing Indicators

library(gplots); 

# load Estimates based on 8 indicators of Lesser Violations CIRI etc

data <- read.csv("figure3_1_04.csv")
summary(data)
# data <- subset(data,    YEAR>=1981 & YEAR<=2010 )
data$latentX <- data$V9


#load Fariss's scores


data1 <- read.csv("HumanRightsProtectionScores_v2.04.csv")
data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )
summary(data1)
nrow(data1)
nrow(data)
data1$latentX <- data1$latentmean

# load Estimates based on 5 indicators of Mass Killings Rommel etc

data3 <- read.csv("figure3_2_04.csv")
summary(data3)
#data3 <- subset(data3,    YEAR>=1981 & YEAR<=2010 )
data3$latentX <- data3$V6


par(new=FALSE)

par(mfrow=c(1,2))

plotmeans(data$latentX  ~ data$YEAR,  ylim=c(-0.2,1),    
          font.main=1, 
          barcol="white", 
          lwd=1,
          pch = 15,
          col="darkblue", cex = 1 ,
          ccol="darkblue",
          n.label="F", 
          xlab ="Year", ylab ="Latent score", 
          cex.lab=1.7, cex.axis=1.8)
text(17, 0.91 ,  cex=1.8 ,col="black", paste("Estimates based on \n 8 indicators of Lesser Violations")) 

par(new=FALSE)

plotmeans(data1$latentX  ~ data1$YEAR,  ylim=c(-0.2,1 ),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 19,
          col="black", cex = 1,
          ccol="white",
          n.label="F", 
          xlab ="Year", ylab ="Latent score", 
          cex.lab=1.7, cex.axis=1.8)
text(51, -0.1  ,  cex=1.8 ,col="black", paste("Fariss's Estimates")) 

par(new=TRUE)



plotmeans(data3$latentX  ~ data3$YEAR, ylim=c(-0.2,1 ),    
          font.main=1, 
          barcol="white", 
          lwd=2,
          pch = 19,
          col="darkblue", cex = 1.1,
          ccol="darkblue",
          n.label="F", 
          xlab ="Year", ylab ="Latent score", 
          cex.lab=1.7, cex.axis=1.8)
text(25, 0.91 ,  cex=1.8 ,col="black", paste("Estimates based on \n 5 indicators of Mass Killings")) 


par(new=FALSE)

