
# iamb method of causal discovery

dsldIamb <- function(data)
{

   getSuggestedLib('bnlearn')
   dsld:::getSuggestedLib('bnlearn')
   return(iamb(data))

}

