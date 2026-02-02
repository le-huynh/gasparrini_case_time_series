#'---
#' title: Note for CTSenvepi example
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
  rio,            # import and export files
  here,           # locate files 
  tidyverse,      # data management and visualization
  gnm,
  dlnm,
  splines,
  skimr
)

#' # Data
# data #-----------
(data <- rio::import(here("note_CTSenvepi/envepi_original.csv")) %>% 
  tibble())

skimr::skim(data)

data %>% count(id)

data %>% count(y)

#' Generate `stratum`
(data <- data %>% 
    mutate(across(.cols = c(id, month, year, dow),
                  as.factor),
           stratum = as.factor(id:year:month)))

#' # Analysis
# analysis #-------------------------

#' ## Splines of time
range(data$date)
diff(range(data$date))
diff(range(data$date))/365.25 * 8
as.numeric(diff(range(data$date))/365.25 * 8)

(dftrend <- round(as.numeric(diff(range(data$date))/365.25 * 8)))

btrend <- ns(data$date, knots = equalknots(data$date, dftrend-1))
head(btrend)

#' ## Cross-basis
#' A `group` argument is used to specify that the variables do not represent a 
#' unique and complete series, but **multiple individual series**

# pollen
cbpoll <- crossbasis(data$pollen,
                     lag = 3,
                     argvar = list(knots = c(40, 100)),
                     arglag = list(knots = 1), 
                     group = data$id)
summary(cbpoll)

# PM2.5
cbpm <- crossbasis(data$pm,
                   lag = 3, 
                   arglag = list("integer"), 
                   group = data$id)
summary(cbpm)

# temperature
cbtmean <- crossbasis(data$tmean,
                      lag = 3, 
                      argvar = list(knots = 1:2 * 10),
                      arglag = list(knots = 1), 
                      group = data$id)
summary(cbtmean)

#' ## Model fitting
#' Fixed-effects logistic regression
mod <- gnm(y ~ cbpoll + cbpm + cbtmean + btrend + dow, 
           eliminate = stratum, 
           data = data,
           family = binomial)
summary(mod)

#' ## Predict the association of the various terms with the risk of respiratory symptoms
cppoll <- crosspred(cbpoll, mod, at = 0:20 * 10, cen = 0)
cppm <- crosspred(cbpm, mod, at = 0:20 * 5, cen = 0)
cptmean <- crosspred(cbtmean, mod, cen = 15, by = 1.5)

#' ## Plot
#' ### Pollen
plot(cppoll, 
     "overall", 
     xlab = "Pollen", 
     ylab = "OR", 
     col = 2,
     main = "Pollen: overall cumulative exposure-response", 
     ylim = c(0.5, 3))

plot(cppoll,
     xlab = "Pollen",
     zlab = "OR",
     main = "Pollen: exposure-lag-response",
     cex.axis = 0.8,
     col = 2)

#' ### PM2.5
plot(cppm,
     var = 10, "overall", 
     xlab = "PM2.5", 
     ylab = "OR", 
     col = 4,
     main = "PM2.5: overall cumulative exposure-response", 
     ylim = c(0.95, 1.20))

plot(cppm,
     var = 10,
     ci = "b",
     type = "p", 
     ylab = "OR", 
     col = 4, 
     pch = 19, 
     cex = 1.7,
     xlab = "Lag", 
     main = "PM2.5: lag-response", 
     lab = c(3, 5, 7), 
     ylim = c(0.995, 1.015))

#' ### Temperature
plot(cptmean, 
     "overall", 
     xlab = "Temperature", 
     ylab = "OR", 
     col = 3,
     main = "Temperature: overall cumulative exposure-response", 
     ylim = c(0.5, 3))

plot(cptmean, 
     xlab = "Temperature", 
     zlab = "OR", 
     ltheta = 240, 
     lphi = 60, 
     cex.axis = 0.8, 
     main = "Temperature: exposure-lag-response", 
     col = 3)

