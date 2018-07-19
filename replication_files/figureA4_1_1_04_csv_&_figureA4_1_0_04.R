# to create data input files figureA4_1_1_04.csv  and figureA4_1_0_04.csv  for figure A4 in the appendix

# there are four blocks of new stardard random data;
# each block is 10 years
# here event data are  as in 2001-2010
# the number of valid observations in the final files reflects the number of missing observations in the original data

# set time start variable
systime1 <- Sys.time()
print(Sys.time() - systime1)
# load libraries
library(rjags); library(coda); library(foreign)

STARTYEAR <- 1949
data1 <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data2 <- subset(data1, data1$YEAR >= STARTYEAR)
data2$YEAR2 <- data2$YEAR

#first block - be careful with time periods and the set value of "block" !!!

start.year <- 2001
end.year <- 2010

# this parameter is different for different time periods
block <- 1

data2 <- subset(data2, data2$YEAR >= start.year & data2$YEAR <= end.year) 
nrow(data2)
for (year in start.year: end.year) {
  data2$YEAR2[data2$YEAR==year] <-  year + block * (1 + end.year - start.year)
}

data2$YEAR <-data2$YEAR2 


data2 <- subset(data2, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty,State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))

RANDOMYEAR <- 1949
#i <-2

set.seed(1234)

#Event
for(i in 2:max(data2$COW)){
  data2$DISAP[!is.na(data2$DISAP) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$DISAP[data2$COW==i & !is.na(data2$DISAP) & data2$YEAR > RANDOMYEAR])
  data2$KILL [!is.na(data2$KILL ) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$KILL [data2$COW==i & !is.na(data2$KILL ) & data2$YEAR > RANDOMYEAR])
  data2$POLPRIS[!is.na(data2$POLPRIS) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$POLPRIS[data2$COW==i & !is.na(data2$POLPRIS) & data2$YEAR > RANDOMYEAR])
  data2$TORT[!is.na(data2$TORT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$TORT[data2$COW==i & !is.na(data2$TORT) & data2$YEAR > RANDOMYEAR])
  
  data2$Amnesty[!is.na(data2$Amnesty) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$Amnesty[data2$COW==i & !is.na(data2$Amnesty) & data2$YEAR > RANDOMYEAR])
  data2$State[!is.na(data2$State) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$State[data2$COW==i & !is.na(data2$State) & data2$YEAR > RANDOMYEAR])
  data2$hathaway[!is.na(data2$hathaway) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$hathaway[data2$COW==i & !is.na(data2$hathaway) & data2$YEAR > RANDOMYEAR])
  data2$ITT[!is.na(data2$ITT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$ITT[data2$COW==i & !is.na(data2$ITT) & data2$YEAR > RANDOMYEAR])
  
  
  
  
  
}



RANDOMYEAR <- 1975
#i <-2

set.seed(1234)


for(i in 2:max(data1$COW)){
  data1$DISAP[!is.na(data1$DISAP) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$DISAP[data1$COW==i & !is.na(data1$DISAP) & data1$YEAR > RANDOMYEAR])
  data1$KILL [!is.na(data1$KILL ) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$KILL [data1$COW==i & !is.na(data1$KILL ) & data1$YEAR > RANDOMYEAR])
  data1$POLPRIS[!is.na(data1$POLPRIS) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$POLPRIS[data1$COW==i & !is.na(data1$POLPRIS) & data1$YEAR > RANDOMYEAR])
  data1$TORT[!is.na(data1$TORT) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$TORT[data1$COW==i & !is.na(data1$TORT) & data1$YEAR > RANDOMYEAR])
  
  data1$Amnesty[!is.na(data1$Amnesty) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$Amnesty[data1$COW==i & !is.na(data1$Amnesty) & data1$YEAR > RANDOMYEAR])
  data1$State[!is.na(data1$State) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$State[data1$COW==i & !is.na(data1$State) & data1$YEAR > RANDOMYEAR])
  data1$hathaway[!is.na(data1$hathaway) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$hathaway[data1$COW==i & !is.na(data1$hathaway) & data1$YEAR > RANDOMYEAR])
  data1$ITT[!is.na(data1$ITT) & data1$COW==i & data1$YEAR > RANDOMYEAR] <- sample(data1$ITT[data1$COW==i & !is.na(data1$ITT) & data1$YEAR > RANDOMYEAR])
  
 
  
}


data <- rbind(data1, data2)
data <- data[order(data$COW,data$YEAR ),]


nrow(data)

#second block

block <- block + 1

data1 <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data2 <- subset(data1, data1$YEAR >= STARTYEAR)
data2$YEAR2 <- data2$YEAR


data2 <- subset(data2, data2$YEAR >= start.year & data2$YEAR <= end.year) 
nrow(data2)

for (year in start.year: end.year) {
  data2$YEAR2[data2$YEAR==year] <-  year + block * (1 + end.year - start.year) #multiplied by block = 2
}

data2$YEAR <-data2$YEAR2


data2 <- subset(data2, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty,State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))

RANDOMYEAR <- 1949
#i <-2

set.seed(123)

#Event
for(i in 2:max(data2$COW)){
  data2$DISAP[!is.na(data2$DISAP) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$DISAP[data2$COW==i & !is.na(data2$DISAP) & data2$YEAR > RANDOMYEAR])
  data2$KILL [!is.na(data2$KILL ) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$KILL [data2$COW==i & !is.na(data2$KILL ) & data2$YEAR > RANDOMYEAR])
  data2$POLPRIS[!is.na(data2$POLPRIS) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$POLPRIS[data2$COW==i & !is.na(data2$POLPRIS) & data2$YEAR > RANDOMYEAR])
  data2$TORT[!is.na(data2$TORT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$TORT[data2$COW==i & !is.na(data2$TORT) & data2$YEAR > RANDOMYEAR])
  
  data2$Amnesty[!is.na(data2$Amnesty) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$Amnesty[data2$COW==i & !is.na(data2$Amnesty) & data2$YEAR > RANDOMYEAR])
  data2$State[!is.na(data2$State) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$State[data2$COW==i & !is.na(data2$State) & data2$YEAR > RANDOMYEAR])
  data2$hathaway[!is.na(data2$hathaway) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$hathaway[data2$COW==i & !is.na(data2$hathaway) & data2$YEAR > RANDOMYEAR])
  data2$ITT[!is.na(data2$ITT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$ITT[data2$COW==i & !is.na(data2$ITT) & data2$YEAR > RANDOMYEAR])
  
 
  
}

data <- rbind(data, data2)
data <- data[order(data$COW,data$YEAR ),]


nrow(data)

# third block

block <- block + 1

data1 <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data2 <- subset(data1, data1$YEAR >= STARTYEAR)
data2$YEAR2 <- data2$YEAR


data2 <- subset(data2, data2$YEAR >= start.year & data2$YEAR <= end.year) 
nrow(data2)

for (year in start.year: end.year) {
  data2$YEAR2[data2$YEAR==year] <-  year + block * (1 + end.year - start.year) #multiplied by block = 3
}

data2$YEAR <-data2$YEAR2


data2 <- subset(data2, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty,State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))

RANDOMYEAR <- 1949
#i <-2

set.seed(12)


for(i in 2:max(data2$COW)){
  data2$DISAP[!is.na(data2$DISAP) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$DISAP[data2$COW==i & !is.na(data2$DISAP) & data2$YEAR > RANDOMYEAR])
  data2$KILL [!is.na(data2$KILL ) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$KILL [data2$COW==i & !is.na(data2$KILL ) & data2$YEAR > RANDOMYEAR])
  data2$POLPRIS[!is.na(data2$POLPRIS) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$POLPRIS[data2$COW==i & !is.na(data2$POLPRIS) & data2$YEAR > RANDOMYEAR])
  data2$TORT[!is.na(data2$TORT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$TORT[data2$COW==i & !is.na(data2$TORT) & data2$YEAR > RANDOMYEAR])
  
  data2$Amnesty[!is.na(data2$Amnesty) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$Amnesty[data2$COW==i & !is.na(data2$Amnesty) & data2$YEAR > RANDOMYEAR])
  data2$State[!is.na(data2$State) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$State[data2$COW==i & !is.na(data2$State) & data2$YEAR > RANDOMYEAR])
  data2$hathaway[!is.na(data2$hathaway) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$hathaway[data2$COW==i & !is.na(data2$hathaway) & data2$YEAR > RANDOMYEAR])
  data2$ITT[!is.na(data2$ITT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$ITT[data2$COW==i & !is.na(data2$ITT) & data2$YEAR > RANDOMYEAR])
  
 
  
}
data <- rbind(data, data2)
data <- data[order(data$COW,data$YEAR ),]


nrow(data)

# fourth block

block <- block + 1

data1 <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data2 <- subset(data1, data1$YEAR >= STARTYEAR)
data2$YEAR2 <- data2$YEAR


data2 <- subset(data2, data2$YEAR >= start.year & data2$YEAR <= end.year) 
nrow(data2)

for (year in start.year: end.year) {
  data2$YEAR2[data2$YEAR==year] <-  year + block * (1 + end.year - start.year) #multiplied by block = 3
}

data2$YEAR <-data2$YEAR2


data2 <- subset(data2, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty,State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))

RANDOMYEAR <- 1949
#i <-2

set.seed(1)


for(i in 2:max(data2$COW)){
  data2$DISAP[!is.na(data2$DISAP) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$DISAP[data2$COW==i & !is.na(data2$DISAP) & data2$YEAR > RANDOMYEAR])
  data2$KILL [!is.na(data2$KILL ) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$KILL [data2$COW==i & !is.na(data2$KILL ) & data2$YEAR > RANDOMYEAR])
  data2$POLPRIS[!is.na(data2$POLPRIS) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$POLPRIS[data2$COW==i & !is.na(data2$POLPRIS) & data2$YEAR > RANDOMYEAR])
  data2$TORT[!is.na(data2$TORT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$TORT[data2$COW==i & !is.na(data2$TORT) & data2$YEAR > RANDOMYEAR])
  
  data2$Amnesty[!is.na(data2$Amnesty) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$Amnesty[data2$COW==i & !is.na(data2$Amnesty) & data2$YEAR > RANDOMYEAR])
  data2$State[!is.na(data2$State) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$State[data2$COW==i & !is.na(data2$State) & data2$YEAR > RANDOMYEAR])
  data2$hathaway[!is.na(data2$hathaway) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$hathaway[data2$COW==i & !is.na(data2$hathaway) & data2$YEAR > RANDOMYEAR])
  data2$ITT[!is.na(data2$ITT) & data2$COW==i & data2$YEAR > RANDOMYEAR] <- sample(data2$ITT[data2$COW==i & !is.na(data2$ITT) & data2$YEAR > RANDOMYEAR])
  
 
  
}

data <- rbind(data, data2)
data <- data[order(data$COW,data$YEAR ),]


nrow(data)


write.csv(as.data.frame(data), "figureA4_1_0_04.csv", row.names=F)

summary(data)

 


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
BURNIN <- 5000
DRAWS <- 25000
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

# j <- dic.samples(m, n.iter=DRAWS, thin=THIN, type="pD")
# j



M <- coda.samples(m, variable.names=c("x", "beta1", "beta3", "beta5", "beta6", "alpha1", "alpha3", "alpha5", "alpha6", "kappa", "sigma"), n.iter=DRAWS, progress.bar="text", thin=THIN)




print(Sys.time() - systime1)



mat1 <- as.matrix(as.mcmc(M[[1]]))
mat2 <- as.matrix(as.mcmc(M[[2]]))
posterior.estimates <- rbind(mat1, mat2)




vars <- t(as.matrix(posterior.estimates))
parameters.means <- apply(vars, 1, mean)
means <- as.matrix(parameters.means)

write.csv(as.data.frame(means), "figureA4_1_1_04.csv", row.names=F)

