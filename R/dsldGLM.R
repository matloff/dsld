# Task 2: linear/generalized linear models:
# Function to be written, we're currently brainstorming things users can utilize.

dsldModel <- function(data, yName, sName, xName, family) {
  return(NULL)
}

# DSLD Function

### What to include::
# 


# No interaction --------------------------------------------------------------------
data(pef)
z <- glm(formula = wageinc ~ ., family = "gaussian", data = pef)
summary(z)


b <- coef(z)
C <- vcov(z)
?vcov()
u <- c(0,0,0,0,1,0,-1,0,0,0,0)

xx <- t(u) %*% C %*% u



colnames(vcov(z))

# Full Interaction -------------------------------------------------------------------
data_split = split(pef,pef[['sex']])
data_split


# male data
pefm <- data_split$`1`
drop <- c("sex")
pefm = pefm[,!(names(pefm) %in% drop)]

# female data
peff <- data_split$`2`
drop <- c("sex")
peff = peff[,!(names(peff) %in% drop)]


m <- glm(formula = wageinc ~ ., family = "gaussian", data = pefm)
f <- glm(formula = wageinc ~ ., family = "gaussian", data = peff)

summary(m)
summary(f)
