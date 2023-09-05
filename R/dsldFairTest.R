dsldFairTest <- function(data, yName, sName, modelFunc, metricFunc, 
                         deweightPars=NULL, cutoff = .5, nReps = 1, 
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
        modelFunc, data=train, yName=yName, sName=sName, deweightPars=deweightPars
        ),
      finally = sink()
    )
    
    # model's prediction
    pred <- predict(model, test)
    if ("probs" %in% names(pred)) test$probs <- pred$probs[,1]
    
    # perform fairness function Metric or 
    # user passed function with access to the model
    Metric <- R.utils::doCall(
      metricFunc, data=test, outcome=yName, group=sName, probs='probs', cutoff=cutoff,
        model=model
      )
    # fairness object has a Metric column
    if ("Metric" %in% names(Metric)) Metric <- t(Metric$Metric)
    
    # manually calculated test accuracy
    predic <- apply(pred$probs, 1, which.max)
    actual <- as.numeric(test[,yName])
    print(cbind(predic, actual))
    error <- mean(predic != actual)
    # for some reason its flipped w/ binary outcome
    if (length(levels(data[,yName])) == 2) error <- mean(predic == actual)
    
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

