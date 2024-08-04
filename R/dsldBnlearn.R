
# iamb method of causal discovery

dsldIamb <- function(data)
{

   getSuggestedLib('bnlearn')
   return(bnlearn::iamb(data))

}

