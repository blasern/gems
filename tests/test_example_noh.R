#################################################################
############################ EXAMPLE ############################
#################################################################
rm(list=ls(all=TRUE))
set.seed(123)

### Load all functions
require(gems)
#source("requiredBL.r")

### Example: input parameters

### Number of states
statesNumber <- 3

### Cohort size
cohortSize <- 1000

### List of hazard functions
hazardf <- generateHazardMatrix(statesNumber)
# transition 1: 1->2: mixed Weibull
hazardf[[1,2]] <- "multWeibull"
# transition 2: 1->3: Weibull, dependent on baseline sex
hazardf[[1,3]] <- function(t, shape, scale, bl) {shape[bl+1]/scale*(t/scale)^(shape[bl+1]-1)}
# transition 3: 2->3  Weibull
hazardf[[2,3]] <- function(t, shape, scale) {shape/sum(1)/scale*(t/scale)^(shape/sum(1)-1)}

### Mean parameter in hazard functions
mu <- generateParameterMatrix(hazardf)
mu[[1,2]] <- list(c(.5,.5),c(1, 5),c(2, 1)) # weights, shapes, scales for transition 1
mu[[1,3]] <- list(c(1,1.5),1) # shapes,scale for transition 2
mu[[2,3]] <- list(2,3) # shape, scale for transition 3

### Covariance matrix of parameters
sigma <-  generateParameterCovarianceMatrix(mu)

### Baseline characteristics
bl <- matrix(rbinom(cohortSize, 1, .5), nrow=cohortSize) # e.g. baseline sex

### time
maxTime <- 10 
by <- .1
times<-seq(0,maxTime,by)

### Simulate the cohort
cohorts <- simulateCohort(
  transitionFunction = hazardf,
  parameters = mu,
  cohortSize = cohortSize,
  parameterCovariance = sigma,
  baseline = bl, 
  to=maxTime
  )

### Print the entry times into the states
print(cohorts)

### Plot the posterior
post <- posteriorProbabilities(cohorts, times)
print(post)