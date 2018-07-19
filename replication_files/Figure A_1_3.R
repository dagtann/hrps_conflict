# figure 4

library(gplots); 

#csv   (same file as for Figure 3 and 4 of the manuscript)

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

nrow(data1)
nrow(data)

data1$latentX <- data1$latentmean

all.data <- cbind(data, x.means)
all.data$x.means <- all.data$V6



countries <-c(unique(data1$COW))

par(mfrow=c(3,3))

page0 <- c(560, 2 ,645 ,   365, 710, 750, 475,  530,     160) #in the text



page1 <- c(40,  140 , 160 , 90, 91, 92, 93,  135,145  ) #Latin America Figure A.1.1

page2 <- c(200, 220,210 , 360, 310, 290,   235,  230 ,339) #Europe 
page3 <- c( 516,482, 484, 490, 500,  437, 438, 483,501) #Africa Figure A.1.2


page5 <- c(  666,  700,   780,  800, 811,  850, 651, 660, 840) #Asia






for(i in page5){
all.datax.means <- subset(all.data,    COW==i  )
data.latentX <- subset(data1,   COW==i )


upperlimit <-4
lowlimit <- -3

par(new=FALSE)




plotmeans(all.datax.means$x.means  ~ all.datax.means$YEAR,  ylim=c(lowlimit, upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=1,
          pch = 19,
          col="blue", cex = 1,
          ccol="blue",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")



par(new=TRUE)

plotmeans(data.latentX$latentX  ~ data.latentX$YEAR, ylim=c(lowlimit, upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=2, 
          pch = 1,
          col="black", cex = 1.1,
          ccol="white",
          
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
legend("topleft", cex = 1.2,  legend = unique(data.latentX$country))



par(new=FALSE)
print(i)

}
par(new=F)


