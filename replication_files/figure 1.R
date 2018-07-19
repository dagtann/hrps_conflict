rm(list = ls())
#Figure 1: A Comparison of the Trends in Mass Killing Events and in Fariss's Latent Scores, 1949-2010

library(gplots);

#load original data from Fariss (2014) for 1949-2010 only

data1 <- read.csv("HumanRightsProtectionScores_v2.04.csv")
data1 <- data1[order(data1$COW ), ]
data1 <- subset(data1,    YEAR>=1949 & YEAR<=2010 )

# keep the original data intact - duplicate working dataset

data.latentX <- data1

# prepare data for the first plot of the figure

data.latentX$events <-0
data.latentX$events[data.latentX$genocide==1] <- 1
data.latentX$events[data.latentX$rummel==1] <- 1
data.latentX$events[data.latentX$massive_repression==1] <- 1
data.latentX$events[data.latentX$executions==1] <- 1
data.latentX$events[data.latentX$killing ==1] <- 1

total <-aggregate(
  data.latentX$events,
  by=list(Category=data.latentX$YEAR),
  FUN=sum
)
summary(total)

total.countries <-aggregate(
  data.latentX$events,
  by=list(Category=data.latentX$YEAR),
  FUN=length
)
percent.total.countries <- 1 -  1 * total$x / total.countries$x
data.latentX$events100 <- 100 * data.latentX$events
no.killings.total <- total.countries$x - total$x
no.killings.total.prop <-  no.killings.total / total.countries$x


# two plots together


par(new=FALSE)

par(mfrow=c(2,1))


plot(total.countries$Category, no.killings.total.prop ,  type="o",  font.main=1,
     main = "Proportion of Countries \ with no Mass KIillings", lwd=2,
     col="black", cex = 1, cex.main=2, cex.axis=1.2, cex.lab=1.5,
     pch=19,
     ccol="black", xlab=" ",
     ylab="proportion of countries", cex.lab=1.5, cex.axis=1.8)


# plot.new()

plotmeans(data1$latentmean ~ data1$YEAR,  xaxt="n" , ylim=c(-0.1, 1),
          font.main=1,
          barcol="white",
          main = "Fariss's Latent Scores",lwd=3, cex.main=2, cex.axis=1.2, cex.lab=1.5,
          pch = 15 ,
          col="black", cex = 1,
          ccol="black",
          n.label="F",
          xlab ="YEAR", ylab ="Latent score",cex.lab=1.7, cex.axis=1.8 )

axis(1, at=(2:62), label=c(1950: 2010))

# plot.new()



par(new=FALSE)



