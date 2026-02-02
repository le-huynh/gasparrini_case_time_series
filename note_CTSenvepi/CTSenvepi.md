CTSenvepi.R
================

```
################################################################################
# Updated version of the R code for the second case-study in:
#
#   "The case time series design"
#   Epidemiology, 2021
#   Antonio Gasparrini
#   http://www.ag-myresearch.com/2021_gasparrini_epidem.html
#
# * an up-to-date version of this code is available at:
#   https://github.com/gasparrini/CaseTimeSeries
################################################################################

```

```
################################################################################
# PREPARATION
################################################################################

```

``` rpackages
library(dlnm) ; library(gnm) ; library(data.table) ; library(splines)
```

``` rsetseed
set.seed(13041975)
par(las=1)
```

```
################################################################################
# SIMULATING THE ORIGINAL DATA
################################################################################

```

``` rsetpartime
n <- 1601
dstart <- as.Date("2015-10-29")
dend <- as.Date("2018-11-19")
date <- seq(dstart, dend, by=1)
year <- year(date)
month <- month(date)
doy <- yday(date)
dow <- factor(wday(date))
```

``` rfutimes
fustart <- sample(seq(dstart, dend-10, by=1), n, replace=TRUE)
fuend <- fustart + pmax(pmin(round(exp(rnorm(n, 5.1, 2))), dend-fustart), 10)
sum(fuend-fustart+1)
```

``` rvartime
cosdoy <- cos(doy*2*pi / 366)
spldate <- bs(date, degree=2, int=TRUE, df=round(length(date)/365.25)*5)
smokeday <- date %in% sample(date[month %in% c(1,2,12)], 20)
```

``` rexpdist
pollen <- exp(cosdoy*2+2.5 + arima.sim(list(ar=0.5), length(date), sd=0.8))
pm <- exp((-cosdoy)*1.6+2.5 + smokeday*3.2 + 
  arima.sim(list(ar=0.6), length(cosdoy), sd=0.95))
tmean <- cosdoy*6+15 + arima.sim(list(ar=0.6), length(cosdoy), sd=2.6)
envdata <- data.frame(date, pollen, pm, tmean)

tibble::tibble(envdata)
```

``` rplotexp
plot(date, pollen, xlab="Date", ylab="Pollen", main="Pollen series", col=2,
  pch=19)
plot(date, pm, xlab="Date", ylab="PM2.5", main="Pollution series", col=2,
plot(date, tmean, xlab="Date", ylab="Temperature", main="Temperature series",
  col=2, pch=19)
layout(1)
```

``` rbaserisk
fortrend <- function(ind=TRUE) (cosdoy*1.6 + sin(doy*4*pi/366))/8+1 + if(ind)
  spldate %*% runif(ncol(spldate),-0.2,0.2) else 0
```

``` rbaseriskplot
plot(date, fortrend(ind=F), type="l", lwd=2, ylim=c(0.5,1.5), xlab="Date",
  ylab="OR", main="Shared baseline risk and indivdual deviations")
abline(h=1)
for(i in 1:7) lines(date, fortrend(ind=T), type="l", lty=2, col=i)
```

``` rorexp
forpoll <- function(x) 1.6 - 0.6*exp(-x/60)
forpm <- function(x) exp(x/1000)
fortmean <- function(x) 1 + ifelse(x>15, 0.002*(x-15)^2, 0)
fwlag <- function(lag) exp(-lag/1.5)
```

``` rplotor
plot(0:200, forpoll(0:200), type="l", xlab="Pollen", ylab="OR",
  main="Exposure-response with pollen", col=2)
plot(0:100, forpm(0:100), type="l", xlab="PM2.5", ylab="OR",
  main="Exposure-response with PM2.5", col=2)
  main="Exposure-response with temperature", col=2)
plot(0:50/10, fwlag(0:50/10), type="l", xlab="Lag", ylab="Weight",
  main="Lag structure", col=2)
layout(1)
```

``` rcumor
exp(sum(log(forpoll(c(50,9, 135, 93))) * fwlag(0:3)))
```

``` rorenv
orpoll <- apply(exphist(pollen, lag=3), 1, function(x) 
  exp(sum(log(forpoll(x)) * fwlag(0:3))))
orpm <- forpm(pm)
ortmean <- apply(exphist(tmean, lag=3), 1, function(x) 
  exp(sum(log(fortmean(x)) * fwlag(0:3))))
orenv <-  orpoll * orpm * ortmean
```

``` rorenvplot
plot(date, ortmean, type="l", xlab="Date", ylab="OR",
  main="Risk of temperature")
  main="Risk of all environmental stressors")
layout(1)
```

``` rsimdata
dlist <- lapply(seq(n), function(i) {
  
  fudate <- seq(fustart[i], fuend[i], by=1)
  sub <- date %in% fudate
  
  ortot <- fortrend(ind=T)[sub] * orenv[sub] * (1 + wday(fudate) %in% c(2:6)*0.4)
  
  pbase <- plogis(-3.3 + 14/length(fudate) - 0.0015*length(fudate))
  sympt <- rbinom(sum(sub), 1, plogis(qlogis(pbase) + log(ortot)))

  data <- cbind(data.frame(id=paste0("sub",sprintf("%04d", i)), date=fudate,
    year=year[sub], month=month[sub], dow=dow[sub], y=sympt), envdata[sub, -1])

  return(data.table(data))
})
data <- do.call(rbind, dlist)

tibble::tibble(data)
dplyr::count(data, id)

rio::export(data,
            here::here("note_CTSenvepi_original/envepi_original.csv"))
```

```
################################################################################
# ANALYSIS
################################################################################

```

``` rsubts
dsub <- subset(data, id=="sub0036")
plot(y~date, data=dsub, type="h", lty=2, ylim=c(0,2), yaxt="n", bty="l", xlab="",
  ylab="Day with \nsymptoms", mgp=c(2.2,0.7,0), lab=c(5,3,7))
points(y~date, data=subset(dsub,y>=1), pch=23, bg=3)
plot(pollen~date, data=dsub, type="l", lty=1, bty="l", col=2, xlab="",
  ylab="Pollen")
  ylab="PM2.5")
plot(tmean~date, data=dsub, type="l", lty=1, bty="l", col=3, xlab="Date",
  ylab="Temperature")
par(mar=c(5,4,4,1)+0.1)
```

``` rprepreg
dftrend <- round(as.numeric(diff(range(data$date))/365.25 * 8))
btrend <- ns(data$date, knots=equalknots(data$date, dftrend-1))
data$stratum <- with(data, factor(paste(id, year, month, sep="-")))
```

``` rcrossbasis
cbpoll <- crossbasis(data$pollen, lag=3, argvar=list(knots=c(40,100)),
  arglag=list(knots=1), group=data$id)
cbpm <- crossbasis(data$pm, lag=3, arglag=list("integer"), group=data$id)
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
  arglag=list(knots=1), group=data$id)
```

``` rmodel
mod <- gnm(y ~ cbpoll + cbpm + cbtmean + btrend + dow, eliminate=stratum, data=data,
  family=binomial)
```

``` rcrosspred
cppoll <- crosspred(cbpoll, mod, at=0:20*10, cen=0)
cppm <- crosspred(cbpm, mod, at=0:20*5, cen=0)
cptmean <- crosspred(cbtmean, mod, cen=15, by=1.5)
```

``` rplotpollen
plot(cppoll, "overall", xlab="Pollen", ylab="OR", col=2,
par(mar=c(1,1,2,1))
layout(1)
par(mar=c(5,4,4,1)+0.1)
```

``` rplotpm
plot(cppm, var=10, "overall", xlab="PM2.5", ylab="OR", col=4,
  main="PM2.5: overall cumulative exposure-response", ylim=c(0.95,1.20))
  xlab="Lag", main="PM2.5: lag-response", lab=c(3,5,7), ylim=c(0.995,1.015))
layout(1)
```

``` rplottmean
plot(cptmean, "overall", xlab="Temperature", ylab="OR", col=3,
par(mar=c(1,1,2,1))
layout(1)
par(mar=c(5,4,4,1)+0.1)
```
