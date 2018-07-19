# figure 4 Farris's Scores Compared to Latent Scores Estimated Using Only Five Indicators of Mass Killing
# nine countries
# more cases are in the online appenix

library(gplots); 

# load Estimates based on 5 indicators of Mass Killings Rommel etc 
# (same file as for  Figure 3)

means <- read.csv("figure3_2_04.csv")


data <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data1 <- read.csv("data1_04.csv")
data1 <- data1[order(data1$COW ), ]
data1 <- data1[order(data1$COW , data1$YEAR   ), ]


nrow(means)
nrowmeans <- nrow(means)
rows.non.x = nrow(means) - nrow(data)
x.means <- as.matrix(means[(rows.non.x+1):nrowmeans,  ])
nrow(x.means)


data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )



data1$latentX <- data1$latentmean

all.data <- cbind(data, x.means)
all.data$x.means <- all.data$V6



countries <-c(unique(data1$COW))

par(mfrow=c(3,3))

page0 <- c(560, 2 ,645 ,   365, 710, 750, 475,  530,     160) #Front page


for(i in page0){
all.datax.means <- subset(all.data,    COW==i  )
data.latentX <- subset(data1,   COW==i )


upperlimit <-4
lowlimit <- -3

par(new=FALSE)

#pdf("plots2.pdf")


plotmeans(all.datax.means$x.means  ~ all.datax.means$YEAR,  ylim=c(lowlimit, upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=2,
          pch = 19,
          col="darkblue", cex = 1,
          ccol="darkblue",
          n.label="F", 
          xlab ="Year", ylab ="Latent scores", 
          cex.lab=1.7, cex.axis=1.8)

#legend("topleft",  legend = unique(all.datax.means$COW))


par(new=TRUE)

plotmeans(data.latentX$latentX  ~ data.latentX$YEAR, ylim=c(lowlimit, upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=1, 
          pch = 1,
          col="black", cex = 1.1,
          ccol="black",
          
          n.label="F", 
          xlab ="Year", ylab ="Latent scores", 
          cex.lab=1.7, cex.axis=1.8)

legend("topleft", bty="n", cex = 2,  legend = unique(data.latentX$country))



par(new=FALSE)
print(i)

}
par(new=F)


