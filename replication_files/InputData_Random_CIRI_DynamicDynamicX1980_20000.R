# set time start variable
systime1 <- Sys.time()
print(Sys.time() - systime1)

# load libraries
library(rjags); library(coda); library(foreign)

STARTYEAR <- 1980
#STARTYEAR <- 1956

#read "transformed" data, with randome variables replacing real values of CIRI data
data <- read.csv("InputDataFariss1949_2010Transformed.csv")

data <- subset(data, data$YEAR >= STARTYEAR)

data <- subset(data, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty, State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))
summary(data)

#here we use only CIRI data and Event Indicators
data <- subset(data, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT,  genocide, rummel, massive_repression, executions, killing))
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

# full model (11 items)
y.pre <- as.matrix(data[,4:12])
y <- matrix(data=NA, ncol=ncol(y.pre), nrow=nrow(y.pre))
for(i in 1:nrow(y.pre)){
  for(j in 1:4){
    y[i, j] <- as.numeric(y.pre[i, j]) + 1
  }
  for(j in 5:9){
    y[i, j] <- as.numeric(y.pre[i, j])  
  }
}


head(y)
time <- data$YEAR - STARTYEAR + 1


# random initial values
MakeInits <- function(){
  nIRT.Binary <- 5
  nIRT.Ordered3 <- 4; Cuts3 <- 2
    
  MU <- matrix(rnorm(length(panel)*max(panel), mean = 0, sd = 1), nrow=length(panel),  ncol=max(panel))

  BETA1 <- runif(nIRT.Binary)
  ALPHA1 <- runif(nIRT.Binary)

  BETA3 <- runif(nIRT.Ordered3)
  ALPHA03 <- array(c(runif(nIRT.Ordered3, 0, 1),
                     runif(nIRT.Ordered3, 1, 2)), dim=c(nIRT.Ordered3, Cuts3, max(year)))

  
  
  SIGMA <- runif(1)

  out <- list(mu=MU, alpha1=ALPHA1, alpha03=ALPHA03, beta1=BETA1, beta3=BETA3, sigma=SIGMA)
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
BURNIN <- 10000
DRAWS <- 20000
THIN <- 10
CHAINS <- 2

NAME <- "CIRI_DynamicDynamicX.bug"

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

#save.image(file="image.Rdata")

# M <- coda.samples(m, variable.names=c("x", "beta1", "beta3", "beta5", "beta6", "alpha1", "alpha3", "alpha5", "alpha6", "kappa", "sigma"), n.iter=DRAWS, progress.bar="text", thin=THIN)

M <- coda.samples(m, variable.names=c("x"), n.iter=DRAWS, progress.bar="text", thin=THIN)


# save.image(file="image.Rdata")


print(Sys.time() - systime1)



mat1 <- as.matrix(as.mcmc(M[[1]]))
mat2 <- as.matrix(as.mcmc(M[[2]]))
posterior.estimates <- rbind(mat1, mat2)
#write.csv(as.data.frame(mat1), "EstimateDynamicStandardDynamicX_YSTAR.csv", row.names=F)
#write.csv(as.data.frame(posterior.estimates), "EstimateDynamicStandardDynamicX.csv", row.names=F)

#save.image(file="image.Rdata")

vars <- t(as.matrix(posterior.estimates))
parameter.mean <- apply(vars, 1, mean)
latentX <- as.matrix(parameter.mean)

all.data <- cbind(data, latentX)
write.csv(as.data.frame(all.data), "InputData_Random_CIRI_DynamicDynamicX1980_20000office.csv", row.names=F)
write.csv(as.data.frame(all.data), "InputData_Random_CIRI_DynamicDynamicX1980_20000office.csv", row.names=T)

