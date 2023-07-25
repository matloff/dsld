getRow1 <- function (data, yName) {
    ycol <- which(names(data) == yName)
    data[1, -ycol]
}

# This function is used by qeFair* function for additional computation
dsldPrepData <- defmacro(zzz,scaling='mmscale',expr=
   {
      ycol <- which(names(data) == yName)
      y <- data[,ycol]

      nonSensNames <- setdiff(names(data),sensNames)
      data1 <- data[nonSensNames]

      ycol <- which(names(data1) == yName)
      x <- data1[,-ycol]
      x <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')

      # scaling
      if (!is.null(scaling)) {
         if (scaling == 'mmscale') {
            xm <- mmscale(x)
            scalePars <- attr(xm,'minmax')
         } else {  # assumed to be ordinary R scale()
            xm <- scale(x)
            scalePars <- list(
                  ctr=attr(xm,'scaled:center'),
                  scl=attr(xm,'scaled:scale')
               )
         }
      }

      xm.df <- as.data.frame(xm)
      data2 <- cbind(xm.df,y)
      names(data2)[1:ncol(xm)] <- colnames(x)

      deweightPars <- expandDeweightPars(data,yName,deweightPars)
      deweightNames <- names(deweightPars)
      deweightVals <- unlist(deweightPars)
   
      trainRow1 <- getRow1(data1,yName)
   }
)
