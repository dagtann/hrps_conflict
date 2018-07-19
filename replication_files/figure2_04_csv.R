
# figure2_04_csv
# set time start variable
systime1 <- Sys.time()
print(Sys.time() - systime1)

# load libraries
library(rjags); library(coda); library(foreign)

STARTYEAR <- 1949
#STARTYEAR <- 1956

#read Fariss data, as in APSR file

data <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.tab")
data <- subset(data, data$YEAR >= STARTYEAR)
data <- subset(data, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty, State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))



set.seed(1234)



#standard

data$DISAP[!is.na(data$DISAP)] <- sample(data$DISAP[!is.na(data$DISAP)])
data$KILL[!is.na(data$KILL)] <- sample(data$KILL[!is.na(data$KILL)])
data$TORT[!is.na(data$TORT)] <- sample(data$TORT[!is.na(data$TORT)])
data$POLPRIS[!is.na(data$POLPRIS)] <- sample(data$POLPRIS[!is.na(data$POLPRIS)])



data$Amnesty[!is.na(data$Amnesty)] <- sample(data$Amnesty[!is.na(data$Amnesty)])
data$State[!is.na(data$State)] <- sample(data$State[!is.na(data$State)])
data$hathaway[!is.na(data$hathaway)] <- sample(data$hathaway[!is.na(data$hathaway)])
data$ITT[!is.na(data$ITT)] <- sample(data$ITT[!is.na(data$ITT)])

summary(data)

nrow(data)
n <- nrow(data)


year <- NA
year[1] <- 1
country <- NA
panel <- NA
panel.count <-1
i <- 2
country[1] <- 1
j <- 1
while(i <= nrow(data)){
if(data$COW[i]!=data$COW[i-1]){
panel[j] <- panel.count
panel.count <- 0
j <- j+1
}
country[i] <- j
#i <- i + 1
panel.count <- panel.count + 1
year[i] <- panel.count
i <- i + 1
}
j
panel[j] <- panel.count

# full model (13 items)
y.pre <- as.matrix(data[,4:16])
y <- matrix(data=NA, ncol=ncol(y.pre), nrow=nrow(y.pre))
for(i in 1:nrow(y.pre)){
  for(j in 1:4){
    y[i, j] <- as.numeric(y.pre[i, j]) + 1
  }
  for(j in 5:8){
    y[i, j] <- as.numeric(y.pre[i, j])
  }
  for(j in 9:13){
    y[i, j] <- as.numeric(y.pre[i, j])
  }
}


head(y)
time <- data$YEAR - STARTYEAR + 1


# random initial values
MakeInits <- function(){
  nIRT.Binary <- 5
  nIRT.Ordered3 <- 4; Cuts3 <- 2
  nIRT.Ordered5 <- 3; Cuts5 <- 4
  nIRT.Ordered6 <- 1; Cuts6 <- 5

  MU <- matrix(rnorm(length(panel)*max(panel), mean = 0, sd = 1), nrow=length(panel),  ncol=max(panel))

  BETA1 <- runif(nIRT.Binary)
  ALPHA1 <- runif(nIRT.Binary)

  BETA3 <- runif(nIRT.Ordered3)
  ALPHA03 <- array(c(runif(nIRT.Ordered3, 0, 1),
                     runif(nIRT.Ordered3, 1, 2)), dim=c(nIRT.Ordered3, Cuts3, max(year)))

  BETA5 <- runif(nIRT.Ordered5)
  ALPHA05 <- array(c(runif(nIRT.Ordered5, 0.0, 0.5),
                      runif(nIRT.Ordered5, 0.5, 1.0),
                      runif(nIRT.Ordered5, 1.0, 1.5),
                      runif(nIRT.Ordered5, 1.5, 2.0)), dim=c(nIRT.Ordered5, Cuts5,  max(year)))

   BETA6 <- runif(nIRT.Ordered6)
  ALPHA06 <- array(c(runif(nIRT.Ordered6, 0.0, 0.5),
                      runif(nIRT.Ordered6, 0.5, 1.0),
                      runif(nIRT.Ordered6, 1.0, 1.5),
                      runif(nIRT.Ordered6, 1.5, 2.0),
                      runif(nIRT.Ordered6, 2.0, 2.5)), dim=c(nIRT.Ordered6, Cuts6,  max(year)))

  SIGMA <- runif(1)

  out <- list(mu=MU, alpha1=ALPHA1, alpha03=ALPHA03, alpha05=ALPHA05,  alpha06=ALPHA06, beta1=BETA1, beta3=BETA3, beta5=BETA5, beta6=BETA6, sigma=SIGMA)
  return(out)
}
inits1 <- MakeInits()
inits2 <- MakeInits()
inits3 <- MakeInits()

inits.function <- function(chain){
  return(switch(chain,
         "1"=inits1,
         "2"=inits2,
         "3"=inits3
         ))
}

# jags.model arguments
ADAPT <- 1000
BURNIN <- 20000
DRAWS <- 100000
THIN <- 10
CHAINS <- 2

NAME <- "LatentRepressionDynamicStandardDynamicX.bug"

# print time taken
print(Sys.time() - systime1)

#rjags code version
m <- jags.model(file=NAME, data=list("y"=y, "time"=time, "year"=year, "country"=country, "n.country"=length(panel), "n.year"=max(panel), "n"=nrow(y)), inits=inits.function, n.chains=CHAINS, n.adapt=ADAPT)

print(Sys.time() - systime1)

update(m, BURNIN)
print(Sys.time() - systime1)


systime1 <- Sys.time()

# j <- dic.samples(m, n.iter=DRAWS,    thin=THIN, type="pD")
# j

#save.image(file="image.Rdata")

M <- coda.samples(m, variable.names=c("x", "beta1", "beta3", "beta5", "beta6", "alpha1", "alpha3", "alpha5", "alpha6", "kappa", "sigma"), n.iter=DRAWS, progress.bar="text", thin=THIN)




print(Sys.time() - systime1)



mat1 <- as.matrix(as.mcmc(M[[1]]))
mat2 <- as.matrix(as.mcmc(M[[2]]))
posterior.estimates <- rbind(mat1, mat2)




vars <- t(as.matrix(posterior.estimates))
parameters.means <- apply(vars, 1, mean)
means <- as.matrix(parameters.means)


parameter.mean <- apply(vars, 1, mean)
parameter.median <- apply(vars, 1, median)
parameter.sd <- apply(vars, 1, sd)
parameter.lower.ci <- apply(vars, 1, quantile, c(0.025))
parameter.upper.ci <- apply(vars, 1, quantile, c(0.975))


all.data <- cbind( parameter.mean, parameter.median, parameter.sd, parameter.lower.ci, parameter.upper.ci)



scores <- as.matrix(all.data)

write.csv(as.data.frame(scores), "figure2_04.csv", row.names=F)

