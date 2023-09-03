### -------------------------------- DsldUtilFairness ----------------------------------------
dsldUtilFairness <- function(data, yName, cName, sName, count = 5, deweight_increment = 0.1) {
  getSuggestedLib('qeML')                                                        
  getSuggestedLib('regtools')
  getSuggestedLib('Kendall')
  
  if (is.factor(data[[cName]])) {
    cGroups <- factorToDummies(data[, cName], cName, omitLast = TRUE)    
    newData <- cbind(data,cGroups)                    
    newData <- newData[,!names(newData) %in% c(cName,sName)]
  } else {
    newData = data
    newData <- newData[,!names(newData) %in% c(sName)]
    cGroups = cName
  }
  
  if (deweight_increment > 0.25) {
    stop(paste("deweight_increment value must be below 0.25"))
  }
  
  d = seq(deweight_increment,1,deweight_increment) 
  
  testAcc = c()  
  mean_cor = c() 
  for (i in d) {
    utils = c()
    fair = c()
    for (k in 1: count) {
      run = qeKNN(newData,yName,expandVars=colnames(cGroups),expandVals=rep(i, length(colnames(cGroups))),scaleX = TRUE)
      test_acc = run$testAcc
      index <- run$holdIdxs 
      subsetted_data = data[index,]
      S_val = subsetted_data[,sName]
      predicted_val = run$holdoutPreds
      v = Kendall(predicted_val,S_val)
      cor_val = v$tau[1]
      utils = c(utils, test_acc)
      fair = c(fair, cor_val)
    }
    mean_util = mean(utils)
    mean_fair = mean(fair)
    
    testAcc = c(testAcc, mean_util)
    mean_cor = c(mean_cor, mean_fair)
  }
  
  # Creating the plot for Utility and Fairness:
  par(mfrow = c(1, 2))
  plot(d,testAcc, type="l", col="red", lwd=1, xlab="D", ylab="Utility (Test Accuracy)")
  title("Utility against D")
  plot(d,mean_cor, type="l", col="blue", lwd=1, xlab="D", ylab="Fairness (Kendall Correlation)")
  title("Fairness against D")
  par(mfrow = c(1, 1))
}

# Example:
# library(regtools); library(Kendall); library(qeML); library(dsld)
# data(svcensus)
# dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'occ', sName = 'gender')     # example w/ categorical cName
# dsldUtilFairness(data = svcensus,yName ='wageinc', cName = 'wkswrkd', sName = 'gender', deweight_increment = 0.05) # example w/ continuous cName
