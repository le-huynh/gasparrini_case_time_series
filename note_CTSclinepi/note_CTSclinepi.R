#'---
#' title: Note for CTSclinepi example
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
  pbs
)

#' # Data
# data #-----------
#' Originial data
(data_orginial <- rio::import(here("note_CTSclinepi/clinepi_original.csv")) %>% 
  tibble())

data_orginial %>% tail(n = 10)

#' Expanded data
#+ warning=FALSE
data_expanded <- rio::import(here("note_CTSclinepi/clinepi_expanded.rds"))

data_expanded %>% enframe()

#' Example of patient sub001 - first 10 rows
data_expanded[[1]] %>% tibble()

#' Example of patient sub001 - last 10 rows
data_expanded[[1]] %>% tibble() %>% tail(n = 10)

#' Data for modelling
data <- data_expanded %>% list_rbind() %>% tibble()

data

data %>% tail(n = 10)

3927*365

#' # Analysis
# analysis #------------------------------
#' Natural cubic splines for `age`
splage <- onebasis(data$age, "ns", knots = quantile(data$age, c(1,3)*0.25))
summary(splage)

#' Cyclic splines for `season`
splseas <- onebasis(data$doy, "pbs", df = 3)
summary(splseas)

#' Distributed lag model (DLM): Cross-basis for `flu` with lag period from 1-91 days
# exposure history
(exphist <- data %>% select(contains("lag")))

# cross-basis
cbspl <- crossbasis(exphist, 
                    lag = c(1, 91), 
                    argvar = list("strata", breaks = 0.5),
                    arglag = list("ns", knots = c(3, 10, 29)))
summary(cbspl)

#' Fitting fixed-effects Poisson regression
mspl <- gnm(y ~ cbspl + splage + splseas,
            data = data,
            family = poisson,
            eliminate = factor(id))

summary(mspl)

#' Predict the association of each term with the risk of AMI
# flu
cpspl <- crosspred(cbspl, mspl, at = 1)
# age
cpsplage <- crosspred(splage, mspl, cen = 70, to = 90)
# season
cpsplseas <- crosspred(splseas, mspl, cen = 366/2, at = 1:365)

#' # Plot
# plot #-----------------------
plot(cpsplage,
     col = 2, 
     ylab = "IRR of AMI", xlab = "Age", main = "Risk by age",
     ylim = c(0, 25))
mtext("Natural cubic splines with 3df", cex = 0.8)

plot(cpsplseas,
     col = 2, 
     ylab = "IRR of AMI", xlab = "Day of the year", main = "Risk by season",
     ylim = c(0.95, 1.30))
mtext("Cyclic splines with 4df", cex = 0.8)

plot(cpspl,
     var = 1, col = 2, 
     ylab = "IRR of AMI", xlab = "Days after a flu episode", main = "Risk by lag",
     ylim = c(0, 5))
mtext("Natural cubic splines with 5df (DLM)", cex = 0.8)

