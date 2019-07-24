#Load library
library(SMCMUNE)

#Load simulated dataset and extract/plot a specific case
alldata = read.csv("SimForce.csv")
stimulus <- as.numeric(alldata[1,-1])
force <- as.numeric(alldata[1+87,-1])  #case contains 5MUs
plot(stimulus, force)

#Format data, apply SMC-MUNE and evaluate post-process correction
data <-  MUNE_data_format(Stimulus = stimulus, Data = force)
FIT  <- MUNE(DATA=data, Urange = 1:7)
FIT  <- MUNE_Orthant(MUNE=FIT, bound = 15)

#Calculate the motor unit number posterior probability mass function
MargLogLike   <- FIT$LPost + FIT$ORTH[,2] - FIT$ORTH[,1]
UPosteriorPMF <- exp(MargLogLike-max(MargLogLike)) / sum(exp(MargLogLike-max(MargLogLike)))
barplot(UPosteriorPMF,ylim=c(0,1))

#Simulate values for observation parameters
paramsU5 <- MUNE_rparam(size=1000, MUNE = FIT, case = 5, order = 1, ECparams = FALSE)
head(paramsU5)


##To sample values for observation AND excitation parametrer, need to first ensure that 
##  the agrument 'Save.GPs=TRUE' is included within MUNE() when fitting the model.
##  This ensures that SMCMUNE saves the relevant information in order to perform the 
##  following command to generage samples from the posterior.

#paramsU5_v2 <- MUNE_rparam(size=1000, MUNE = FIT, case = 5, order = 1, ECparams = TRUE)
#head(paramsU5_v2)
