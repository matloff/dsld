dsldFairTest <- function(data, yName, sName, modelFunc, metricFunc, 
                         ..., cutoff = .5, nReps = 1, 
                         testProportion = 0.3) {
  dsld::getSuggestedLib("R.utils")
  
  singleTest <- function() {
    # partition the data
    partition <- sample(1:nrow(data))[1:round(nrow(data) * testProportion)]
    train <- data[-partition, ]
    test <- data[ partition, ]
    
    # avoid warning statements since these are used in the book
    sink(nullfile())
    
    # use tryCatch so we can reset the sink if the qeFunc fails
    model <- tryCatch(
      # right now only tested to work with dsldQeFairRF and qeRFranger
      # doCall used to work with unused args
      R.utils::doCall(
        modelFunc, data=train, yName=yName, sName=sName, yesYVal = "Yes", 
        ...
      ),
      finally = sink()
    )
    
    # model's prediction
    prediction <- predict(model, test)
  
    # EDFfair wrapper models have a probs attribute
    if ("probs" %in% names(prediction)) {
      test$probs <- prediction$probs[,1]
      preds <- apply(prediction$probs, 1, which.max) # used for misclass error
    } 
    # fairml wrapper models dont
    else {
      test$probs <- prediction
      preds <- (prediction > cutoff) + 1 # used for misclass error
    }
    
    # perform fairness function Metric or 
    # user passed function with access to the model
    Metric <- R.utils::doCall(
      metricFunc, data=test, outcome=yName, group=sName, probs='probs', cutoff=cutoff,
        model=model
      )
    # fairness object has a Metric column
    if ("Metric" %in% names(Metric)) Metric <- t(Metric$Metric)
    
    # manually calculated test accuracy
    actual <- as.numeric(test[,yName])
    error <- mean(preds != actual)
    
    # append test accuracy to the output
    Metric <- cbind(Metric, NA)
    Metric[1, ncol(Metric)] <- error
    colnames(Metric)[ncol(Metric)] <- "Misclass Error"
    Metric
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
# dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 2)
# 
# dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, \(model) model$corrsens, nReps = 2)

