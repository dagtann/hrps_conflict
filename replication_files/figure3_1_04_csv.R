# to create figure3_1_04_csv
# no_event

systime1 <- Sys.time()
print(Sys.time() - systime1)

# load libraries
library(rjags); library(coda); library(foreign)

STARTYEAR <- 1976
#STARTYEAR <- 1956


data <- read.csv("FarissAPSRData1949_2010_prepared_for_analysis_04.csv")
data <- subset(data, data$YEAR >= STARTYEAR)
data <- subset(data, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty, State, hathaway, ITT, genocide, rummel, massive_repression, executions, killing))


data <- subset(data, select=c(YEAR, CIRI, COW, DISAP, KILL, POLPRIS, TORT, Amnesty, State, hathaway, ITT))

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

# full model (6 items)
y.pre <- as.matrix(data[,4:11])
y <- matrix(data=NA, ncol=ncol(y.pre), nrow=nrow(y.pre))
for(i in 1:nrow(y.pre)){
  for(j in 1:4){
    y[i, j] <- as.numeric(y.pre[i, j]) + 1
  }
  for(j in 5:8){
    y[i, j] <- as.numeric(y.pre[i, j])
  }

}



head(y)
time <- data$YEAR - STARTYEAR + 1



# random initial values
MakeInits <- function(){
  
  nIRT.Ordered3 <- 4; Cuts3 <- 2
  nIRT.Ordered5 <- 3; Cuts5 <- 4
  nIRT.Ordered6 <- 1; Cuts6 <- 5
  
  MU <- matrix(rnorm(length(panel)*max(panel), mean = 0, sd = 1), nrow=length(panel),  ncol=max(panel))
  
  
  
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
  
  out <- list(mu=MU, alpha03=ALPHA03, alpha05=ALPHA05,  alpha06=ALPHA06, beta3=BETA3, beta5=BETA5, beta6=BETA6, sigma=SIGMA)
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

NAME <- "NoEventDynamicDynamic.bug"

# print time taken
print(Sys.time() - systime1)

#rjags code version
m <- jags.model(file=NAME, data=list("y"=y, "time"=time, "year"=year, "country"=country, "n.country"=length(panel), "n.year"=max(panel), "n"=nrow(y)), inits=inits.function, n.chains=CHAINS, n.adapt=ADAPT)

print(Sys.time() - systime1)

update(m, BURNIN)
print(Sys.time() - systime1)


systime1 <- Sys.time()

M <- coda.samples(m, variable.names=c("x"), n.iter=DRAWS, progress.bar="text", thin=THIN)


#save.image(file="image.Rdata")


print(Sys.time() - systime1)
mat1 <- as.matrix(as.mcmc(M[[1]]))
mat2 <- as.matrix(as.mcmc(M[[2]]))
posterior.estimates <- rbind(mat1, mat2)
#write.csv(as.data.frame(mat1), "EstimateDynamicStandardDynamicX_YSTAR.csv", row.names=F)
#write.csv(as.data.frame(posterior.estimates), "EstimateDynamicStandardDynamicX.csv", row.names=F)

#save.image(file="image.Rdata")

vars <- t(as.matrix(posterior.estimates))
parameter.mean <- apply(vars, 1, mean)
scoret <- as.matrix(parameter.mean)

ally.pre <- cbind(y.pre, scoret)
all.data <- cbind(data, ally.pre)
write.csv(as.data.frame(all.data), "figure3_1_04.csv", row.names=F)


