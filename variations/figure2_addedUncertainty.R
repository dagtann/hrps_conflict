library('ggplot2')
alpha <- .05

pdta <- subset(
  all.data,
  select = c("YEAR", "COW", "parameter.mean", "parameter.sd")
)
pdta[, "type"] <- "replication"

tmp <- subset(
  data1, select = c("YEAR", "COW", "latentmean", "latentsd")
)
names(tmp)[3:4] <- c("parameter.mean", "parameter.sd")
tmp[, "type"] <- "original"
pdta <- rbind.data.frame(pdta, tmp); rm(tmp)
pdta <- within(pdta, {
  n_by_year <- ave(COW, YEAR, type, FUN = length)
  parameter.sd_yr <- ave(
    parameter.sd, YEAR, type, FUN = function(x){
      n <- length(x)
      sqrt(sum((1/n)^2 * x^2))
    }
  )
  }
)
pdta <- aggregate(
  x = pdta[, c('parameter.mean', 'parameter.sd_yr')],
  by = list(type = pdta[['type']], year = pdta[['YEAR']]),
  FUN = mean
)
pdta <- within(pdta, {
  lower <- qnorm(p = alpha/2, parameter.mean, parameter.sd_yr)
  upper <- qnorm(p = 1 - alpha/2, parameter.mean, parameter.sd_yr)
  }
)
ggplot(data = pdta, aes(x = year, y = parameter.mean, group = type, ymin = lower, ymax = upper)) +
 geom_ribbon(alpha = .2) +
 geom_line(aes(colour = type))
