#A4 The more future records of mass killings, 
# the less the future improvement in human rights Fariss's latent scores would reveal. 
#that there would be a strong improving trend in Fariss's latent scores 
#if the incidents of mass killing follow the same pattern 
#as it was between 2001 and 2010. 
#there still would be steady improvement in Fariss's latent scores 
#even if the incidents of mass killings after 2010 
#follow the same pattern as it was between 1991 and 2000.  
#there will be NO improvement in Fariss's latent scores 
#if the incidents of mass killings after 2010 follow the same pattern 
#as it was between 1981 and 1990.  

library(gplots); 

#load scores based on alternative scenarios

means <- read.csv("figureA4_1_1_04.csv") 
means1 <- read.csv("figureA4_2_1_04.csv")
means2 <- read.csv("figureA4_3_1_04.csv")

#load data files used to calculate the alternative scores 



matrix.compare <- read.csv("figureA4_1_0_04.csv")
matrix.compare1 <- read.csv("figureA4_2_0_04.csv")
matrix.compare2 <- read.csv("figureA4_3_0_04.csv")


# this step is to exract from the files only the score variables and to drop all other estimations
nrow(means)
nrowmeans <- nrow(means)
rows.non.x = nrow(means) - nrow(matrix.compare)
x.means <- as.matrix(means[(rows.non.x+1):nrowmeans,  ])
nrow(x.means)

all.data <- cbind(matrix.compare, x.means)


nrow(means1)
nrowmeans1 <- nrow(means1)
rows.non.x1 = nrow(means1) - nrow(matrix.compare1)
x.means1 <- as.matrix(means1[(rows.non.x1+1):nrowmeans1,  ])
nrow(x.means1)

all.data1 <- cbind(matrix.compare1, x.means1)

nrow(means2)
nrowmeans2 <- nrow(means2)
rows.non.x2 = nrow(means2) - nrow(matrix.compare2)
x.means2 <- as.matrix(means2[(rows.non.x2+1):nrowmeans2,  ])
nrow(x.means2)

all.data2 <- cbind(matrix.compare2, x.means2)


# set  plot parameter to be the same for three lines
upperlimit <-1.7
lowlimit <--0.5

par(new=FALSE)



plotmeans(all.data1$x.means  ~ all.data1$YEAR,  ylim=c(lowlimit,upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=1,
          pch = "o",
          col="red", cex = 1,
          ccol="red",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
text(91, 1.2  ,col="red", cex = 1.2, paste("If Mass Killing events would repeat \n the pattern of 1991-2000")) 
abline(v=62)
#abline(v=43)


par(new=TRUE)




plotmeans(all.data2$x.means  ~ all.data2$YEAR,  ylim=c(lowlimit,upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=1,
          pch = 19 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
text(86, 0.3  ,col="black", cex = 1.2, paste("If Mass Killing events would repeat the pattern of 1981-1990")) 
abline(v=62)
#abline(v=43)


par(new=TRUE)




plotmeans(all.data$x.means  ~ all.data$YEAR,  ylim=c(lowlimit,upperlimit),  
          font.main=1, 
          barcol="white", 
          lwd=1,
          pch = 19,
          col="blue", cex = 1,
          ccol="blue",
          n.label="F", 
          xlab ="Year", ylab ="Latent Scores")
text(88, 1.7  ,col="blue", cex = 1.2, paste("If Mass Killing events would repeat the pattern of 2001-2010")) 
abline(v=62)
#abline(v=43)
abline(h=0.5)
par(new=TRUE)






