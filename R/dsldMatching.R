# Estimate the average treatment effect using covariate or propensity matching.
# S identifies treatment/exposure; yesYVal selects the positive binary outcome.
dsldMatchedATE <- function(data,yName,sName,yesSVal,yesYVal=NULL,
   propensFtn=NULL,k=NULL)
{
   getSuggestedLib("Matching")
   .dsld_check_data(data, c(yName, sName), complete = TRUE)
   data <- toNumericFactor(data)
   if (!is.null(propensFtn)) propensFtn <- match.arg(propensFtn, c("glm", "knn"))
   treatment <- .dsld_binary_response(data[[sName]], yesSVal)
   response <- data[[yName]]
   if (is.factor(response) || !is.null(yesYVal)) {
      response <- .dsld_binary_response(response, yesYVal)
   }
   if (!is.numeric(response)) stop("Y must be numeric or a binary factor.")

   covariates <- data[, setdiff(names(data), c(yName, sName)), drop = FALSE]
   if (!ncol(covariates)) stop("At least one matching covariate is required.")
   if (!allNumeric(covariates)) {
      matching_values <- factorsToDummies(covariates, omitLast = TRUE, dfOut = TRUE)
   } else matching_values <- as.matrix(covariates)

   if (identical(propensFtn, "glm")) {
      matching_values <- glm(treatment ~ matching_values, family = binomial)$fitted.values
   } else if (identical(propensFtn, "knn")) {
      if (is.null(k)) k <- 25L
      propensity_model <- qeKNN(data[, setdiff(names(data), yName), drop = FALSE],
         sName, yesYVal = yesSVal, k = k, holdout = NULL)
      matching_values <- propensity_model$regests
   }
   Matching::Match(Y = response, Tr = treatment, X = matching_values,
                   estimand = "ATE", ties = FALSE)
}
