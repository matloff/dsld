
# ad hoc aid in deciding which covariates one should treat as
# proxies

# we want to find variables O that are correlated with S; S need not be
# binary/categorical

# based on cor(), using Kendall's Tau in order to acccomdate binary
# variables (0,1 valued), and to mitigate effects of outliers

dsldOHunting <- function(data,yName,sName) 
{

   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)

   sdumms <- regtools::factorsToDummies(data[,scol,drop=FALSE])
   odumms <- regtools::factorsToDummies(data[,-c(ycol,scol),drop=FALSE])

   cor(sdumms,odumms,method='kendall')

}

