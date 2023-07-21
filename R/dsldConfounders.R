#' @examples 
#' library(dsld)
#' data(svcensus)
#' dsldConfounders(svcensus)
dsldConfounders <- function(data, fill = FALSE) {
    library(dsld)
    for (i in 1:ncol(data)) {
        if (is.factor(data[,i])) {
            print("categorical")
            print(colnames(data)[i])
        }
        else if (is.numeric(data[,i])) {
            # print("numeric")
            # print(colnames(data)[i])
            dsldDensityByS(data,colnames(data)[i],"educ",fill)
        }
        else {
            print("Not categorical or numeric")
        }
    }
}
library(dsld)
data(svcencus)
dsldConfounders(svcensus)