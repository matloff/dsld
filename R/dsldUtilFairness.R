#' TODO: Modify function to use plotly instead of plot() to benefit Py users
#'
#' ::: Description :::
#' @brief Depicts the utility and fairness against D
#'          in graphical form
#'
#' ::: Arguments :::
#' @param data: dataset, an R dataframe
#' @param yName: name of the response column [character]
#' @param cName:
#' @param sName: name of the sensitive variable, an R factor
#' @param count: Number of times to run the qeKNN function call
#'
dsldUtilFairness <- function(data, yName, cName, sName, count = 25) {
    getSuggestedLib('qeML') # load necessary libraries
    getSuggestedLib('regtools')
    getSuggestedLib('Kendall')
    
    # data pre-processing
    cGroups <- factorToDummies(data[, cName], cName, omitLast = TRUE) # create dummy variables for cName
    newData <- cbind(data,cGroups) # create new dataframe w/ dummy variables
    newData <- newData[,!names(newData) %in% c(cName,sName)] # remove cName (categorical column) and sName
    # View(data)
    # View(newData)
    
    # create values for d to be tested
    d = seq(0.1,1,0.1) # 0-1; intervals of 0.1 (10 values)
    
    # measure utility and fairness
    testAcc = c()  # initialize empty vectors 
    mean_cor = c() 
    for (i in d) {
        # utility
        val = replicMeans(count,"qeKNN(newData,yName,expandVars=colnames(cGroups),expandVals=rep(i, length(colnames(cGroups))),scaleX = TRUE)$testAcc")
        testAcc = c(testAcc, val)
        
        # fairness
        cor_vector = c() # computes 25 correlation values across every i-th run
        for (j in 1:count) { # this for loop is intended to run each iteration of d 25 times (like replicMeans) and compute the overall mean from all runs
            run <- qeKNN(newData,yName,expandVars=colnames(cGroups),expandVals=rep(i, length(colnames(cGroups))),scaleX = TRUE) # run the qeKNN function j times for each i-th iteration
            index <- run$holdIdxs # get index values of holdout set 
            S_val = data[index,sName] # get S values of holdout set from original data
            predicted_val = run$holdoutPreds # get predicted values from holdout set
            v = Kendall(predicted_val,S_val) # compute Kendall correlation
            cor_val = v$tau[1] # store correlation
            cor_vector = c(cor_vector, cor_val) # append cor to vector
        }
        mean_val = mean(cor_vector)
        mean_cor = c(mean_cor, mean_val)
    }
    
    # Creating the plot for Utility and Fairness:
    par(mfrow = c(1, 2))
    plot(d,testAcc, type="l", col="red", lwd=1, xlab="D", ylab="Utility (Test Accuracy)")
    title("Utility against D")
    plot(d,mean_cor, type="l", col="blue", lwd=1, xlab="D", ylab="Fairness (Kendall Correlation)")
    title("Fairness against D")
    par(mfrow = c(1, 1))
    
    ### Although our results varied each run, we found that our utility was usually lowest at d = 0.6 and our fairness 
    ### decreased with increasing d. 
}

# Examples
# library(regtools); library(Kendall); library(qeML)
# data(pef)
# dsldUtilFairness(pef, 'wageinc', 'occ', 'sex', 5)

    

