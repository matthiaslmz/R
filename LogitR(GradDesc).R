#source 1:http://czep.net/stat/mlelr.pdf
#source 2:http://www.win-vector.com/blog/2011/09/the-simpler-derivation-of-logistic-regression/
library(OpenMx)

###Estimate the coefficient(s) for a logistic regression model by optimizing the maximum likelihood function (use gradient descent)
df <- read.csv('BreastCancer.csv')
table(is.na(df))
df <- na.omit(df)
head(df)
ncol(df)
y <- as.matrix(df[,11],rownames.force = NA) #response variable
x <- as.matrix(df[,2:10],rownames.force = NA) #predictor variables
x <- cbind(1,x) #adding intercept or bias term.
xT <- t(x) # transpose x 

n <- nrow(df)

#The logistic function
Px <- function(b) {
  z <- x %*% b               #summing
  exp(z)/(1+exp(z))
}

#Minimizing function so that we know when we have approximately reached our target (minima)
ssquares <- function(logit1,logit2) {
  return(sum(abs(logit1 - logit2)))
}

#Diagonal matrix W of n x n
W <- function(Px) {
  vec2diag(Px*(1-Px))
}

#Define mu
Mu <- function(Px)
{
  Px
}

#Function to compute logit
logit <- function(Px){
  log(Px/(1-Px))
}


#Newton-Raphson Method for Logistic Regression
#This method takes in 2 parameters. First param is to initialize our betas (10 in total) by using rep(0,10)
#Second param is a tolerance param that helps us to gauge when to stop approximating and we know we reached minima.
NewtonsRaphson <- function(betas,tol) { 
  
  #initial values
  prev_B <- betas
  prev_Px <- Px(prev_B)
  prev_logit <- logit(prev_Px)
  
  #next iteration values
  curr_B <- prev_B + solve(xT %*% W(prev_Px) %*% x) %*% xT %*% (y - Mu(prev_Px))
  curr_Px <- Px(curr_B)
  curr_logit <- logit(curr_Px)
  
  while (ssquares(prev_logit, curr_logit) > tol) {
    prev_B <- curr_B
    prev_Px <- Px(prev_B)
    prev_logit <- logit(prev_Px)
    
    curr_B <- prev_B + solve(xT %*% W(prev_Px) %*% x) %*% xT %*% (y - Mu(prev_Px))
    curr_Px <- Px(curr_B)
    curr_logit <- logit(curr_Px)
    #print(curr_logit)
  }
  return(curr_B)
}

NewtonsRaphson(rep(0,10),tol=1e-11)

###---Check using R logit function---###
mylogit <- glm(Class ~. , data = df, family = "binomial")
summary(mylogit)

