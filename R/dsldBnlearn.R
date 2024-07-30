
# iamb method of causal discovery

dsldIamb <- function(data)
{

   getSuggestedLib('bnlearn')
   dsld:::getSuggestedLib('bnlearn')
   return(bnlearn::iamb(data))

}

