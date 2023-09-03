dsldFairTest <- function(data, yName, sName, qeFunc, fairFunc, 
                         deweightPars=NULL, cutoff = .5, nReps = 1, 
                         testProportion = 0.3, t = FALSE) {
  dsld::getSuggestedLib("fairness")
  
  singleTest <- function() {
    # partition the data
    partition <- sample(1:nrow(data))[1:round(nrow(data) * testProportion)]
    train <- data[-partition, ]
    if (t) train[,sName] <- unique(train[,sName])[1]
    test <- data[ partition, ]
    
    # avoid warning statements since these are used in the book
    sink(); sink(nullfile())
    
    # right now this only works with dsldQeFairRF
    model <- R.utils::doCall(qeFunc, data=train, yName=yName, sName=sName,
                                deweightPars=deweightPars)
    
    sink()
    # model's prediction
    test$probs <- predict(model, test)$probs[,1]
    
    # perform fairness function metric
    dem <- fairFunc(test, yName, sName, 'probs', 
                cutoff = cutoff)
    
    # append test accuracy to the output
    MisclassRate <- model$testAcc
    dem$Metric <- cbind(dem$Metric, MisclassRate)
    dem$Metric
  }
  
  # repeats the calculation nreps times and averages the data tables
  suppressWarnings({
    apply(replicate(nReps, singleTest()), c(1,2), mean, na.rm=TRUE)
  })
}

# ---- test -----

# data <- fairml::compas
# yName <- 'two_year_recid'
# sName <- 'race'
# 
# dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 5)
# 
# deweightPars = list(decile_score=0, priors_count=0)
# dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 5, deweightPars = deweightPars)

