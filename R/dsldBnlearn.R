
# iamb method of causal discovery

dsldIamb <- function(data)
{

   dsld:::getSuggestedLib('bnlearn')
   return(iamb(data))

}

