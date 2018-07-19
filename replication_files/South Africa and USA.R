#plot random

library(gplots); 

# load Fariss's scores and country names


data1 <- read.csv("data1_04.csv")
data1 <- data1[order(data1$COW, data1$YEAR   ), ]

year <- 1949:2010
set.seed(10)

(mfrow=c(1,1))
par(new=FALSE)

dataMongol <- subset(data1,  country == "South Africa" & YEAR>=1949 & YEAR<=2010)
#plot(y=dataBelgium$latentmean,x=dataZealand$YEAR , cex.lab=1,  main = "New Zealand",   ylab = "Dynamic Latent Human Rights",  xlab = "Year", ylim=range(0:4))



latentmean <-c(dataMongol$latentmean)
latentsd    <-c(dataMongol$latentsd )




# create a matrix to hold 5000 draws from each country-year distribution for Guatemala (the draws are the rows and the country years are the columns)
draws <- matrix(NA, nrow=500 , ncol=length(year))

# take 5000 draws from a normal distribution for each year of the data, which fills 5000 rows in each i column of the data
for(i in 1:length(year)){
  draws[,i]  <- rnorm(500, mean=latentmean[i], sd=latentsd[i])
}


# set parameters for the plotting device 
#par(mfrow=c(1,1))
#par(mar=c(2,3,0.2,0), cex=1.2, font=2, font.lab=2)


# calculate the mean value of each column in the matrix; i.e., the mean of each column of 5000 draws 
point.estimates <- apply(draws, 2, mean)
sapoint.estimates <- apply(draws, 2, quantile, .025)


# plot the country-year point.estimates 
plot(point.estimates, ylim=c(-3.0,4.0), yaxt="n", xaxt="n", pch=19, col="lightsteelblue4", xlab="", ylab="")
axis(1, at=(1:length(year)), label=c(1949:2010))
axis(2, at=(-3:3), las=2)
mtext(side=2, "Latent Human Rights Estimates", line=2, cex=1.15)


# add 95% credible intervals to the plot
for(i in 1:length(year)){
  lines(c(i,i), c(apply(draws, 2, quantile, .025)[i], apply(draws, 2, quantile, .975)[i]), lwd=4, col="lightsteelblue4")
}
points(apply(draws, 2, mean), col="lightsteelblue4", pch=19)


# determine which year is the minimum value on the latent variable and which year is the maximum value
minyear <- which(apply(draws, 2, mean)==min(apply(draws, 2, mean)))
maxyear <- which(apply(draws, 2, mean)==max(apply(draws, 2, mean)))


# color code the year with the maximum estimated value and the year with lowest
#points(minyear, apply(draws, 2, mean)[minyear], col=2, pch=19)
#points(maxyear, apply(draws, 2, mean)[maxyear], col=3, pch=19)
#lines(c(minyear,minyear), c(apply(draws, 2, quantile, .025)[minyear], apply(draws, 2, quantile, .975)[minyear]), lwd=4, col=2)
#lines(c(maxyear,maxyear), c(apply(draws, 2, quantile, .025)[maxyear], apply(draws, 2, quantile, .975)[maxyear]), lwd=4, col=3)


# add the country name to the plot
legend(x=-1, y=3.9, legend=c("Mongolia"), bty="n", cex=1.4)

par(new=TRUE)


#USA

dataUSA   <- subset(data1,  country == "United States" & YEAR>=1949 & YEAR<=2010)
#plot(y=dataNetherlands$latentmean,x=dataNetherlands$YEAR , cex.lab=1,  main = "Australia",  ylab = "Dynamic Latent Human Rights",  xlab = "Year", ylim=range(0:4))


latentmean <-c(dataUSA$latentmean)
latentsd    <-c(dataUSA$latentsd )




# create a matrix to hold 5000 draws from each country-year distribution for Guatemala (the draws are the rows and the country years are the columns)
draws <- matrix(NA, nrow=500, ncol=length(year))

# take 5000 draws from a normal distribution for each year of the data, which fills 5000 rows in each i column of the data
for(i in 1:length(year)){
  draws[,i]  <- rnorm(500, mean=latentmean[i], sd=latentsd[i])
}


# set parameters for the plotting device 

#par(mar=c(2,3,0.2,0), cex=1.2, font=2, font.lab=2)


# calculate the mean value of each column in the matrix; i.e., the mean of each column of 5000 draws 
point.estimates <- apply(draws, 2, mean)
sapoint.estimates <- apply(draws, 2, quantile, .025)


# plot the country-year point.estimates 
plot(point.estimates, ylim=c(-3.0,4.0), yaxt="n", xaxt="n", pch=19, col="blue", xlab="", ylab="")
axis(1, at=(1:length(year)), label=c(1949:2010))
axis(2, at=(-3:3), las=2)
mtext(side=2, "Latent Human Rights Estimates", line=2, cex=1.15)


# add 95% credible intervals to the plot
for(i in 1:length(year)){
  lines(c(i,i), c(apply(draws, 2, quantile, .025)[i], apply(draws, 2, quantile, .975)[i]), lwd=4, col="lightsteelblue4")
}
points(apply(draws, 2, mean), col="blue", pch=19)


# determine which year is the minimum value on the latent variable and which year is the maximum value
minyear <- which(apply(draws, 2, mean)==min(apply(draws, 2, mean)))
maxyear <- which(apply(draws, 2, mean)==max(apply(draws, 2, mean)))


# color code the year with the maximum estimated value and the year with lowest
points(minyear, apply(draws, 2, mean)[minyear], col=2, pch=19)
points(maxyear, apply(draws, 2, mean)[maxyear], col=3, pch=19)
lines(c(minyear,minyear), c(apply(draws, 2, quantile, .025)[minyear], apply(draws, 2, quantile, .975)[minyear]), lwd=4, col=2)
lines(c(maxyear,maxyear), c(apply(draws, 2, quantile, .025)[maxyear], apply(draws, 2, quantile, .975)[maxyear]), lwd=4, col=3)


# add the country name to the plot
legend(x=-1, y=-2.2, legend=c("USA"), bty="n", cex=1.4)

par(new=FALSE)

