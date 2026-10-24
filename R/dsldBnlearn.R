
# Conditional-independence structure discovery using bnlearn's IAMB method.

dsldIamb <- function(data)
{

   getSuggestedLib('bnlearn')
   .dsld_check_data(data, names(data), complete = TRUE)
   # bnlearn distinguishes continuous doubles from integer columns.
   return(bnlearn::iamb(toNumericFactor(data)))

}

