#figure 2

library(gplots);

#load Estimates from Random Values of ALL Lesser Violations

means <- read.table("figure2_04.tab", head = TRUE, sep = "\t")

#load original data to combine with random estimates

data <- read.table(
     "Fake_CIRI_FarissAPSRData1949_2010_prepared_for_analysis_04.tab",
     head = TRUE, sep = "\t"
)

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
data1 <- subset(data1, YEAR>=1949 & YEAR<=2010)
head(data1[data1$YEAR == 1949, ])
#plot figure 2

par(new=FALSE)

plotmeans(data1$latentmean  ~ data1$YEAR, ylim=c(-0.2, 1 ),
          font.main=1,
          barcol="white",
          lwd=4,
          lty =5,
          pch = "-",
          col="black", cex = 1.5,
          ccol="black",
          n.label="F",
          xlab ="Year", ylab ="Latent scores",
          cex.lab=1.8, cex.axis=1.8)

#legend(45, 0.2, "Fariss's Scores", bty="n" , cex=1.5);

par(new=TRUE)

plotmeans(all.data$parameter.mean  ~ all.data$YEAR, ylim=c(-0.2, 1 ),
          font.main=1,
          barcol="white",
          lwd=5,
          lty =1,
          pch = "-",
          col="darkblue", cex = 1.2,
          ccol="darkblue",
          n.label="F",
          xlab ="Year", ylab ="Latent scores",
          cex.lab=1.8, cex.axis=1.8)


#legend(1, 0.9, "Estimates from Random Values of ALL Lesser Violations",  bty="n" , cex=1.5);
par(lwd = 3)
legend("topleft", legend=c("Estimates from Random Values of ALL Lesser Human Rights Violations", "Fariss's Scores"),
       col=c("blue", "black"), bty="n" ,lty=1:2 , cex=1.9)

par(new=FALSE)


