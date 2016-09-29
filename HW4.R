#Homework 4 
#Paul Bouche
#29/09

require(MASS)
###########
#Question 1
###########

#A classic regression model is written matricially: 

# y = XB +e 
#with y a vector that contains the observations of the variable we want to 
#explain, X a matrix that contains observations of explanatory variables, 
# B (beta) the vector we want to estimate and e the vector of errors. 

# For each individual i we have, with only one explanatory variable: 

#y_i = b_0 + b_1.x_i +e_i 

#To fit the matricial writing of this problem, the matrix X has to be: 

# ( 1 x_1  )
#    ....
# ( 1 x_n  )

#However, in the modelization we performed earlier, there is no column of "1",
#Hence b_0 is not estimated. 

#On the software side, the lm() function considers by default that b_0 has to
#be estimated. That is why the results made "by hand" are differents from those 
#with lm() function.



###########
#Question 2
###########


# Set the correlation parameter and mean
beta = 0.5
SIGMA = matrix(c(1,beta,beta,1), ncol=2)
MU = c(2.0, 1.0)

# Set the sample size
N = 50

# Draw the sample
out <- mvrnorm(N, mu = MU, Sigma = SIGMA)

plot(out)
abline(lm(out[,2]~out[,1]), col="red")


# Our data set is named `out`, which we split into y and X
y <- out[, 2]
X <- cbind(rep(1, N),out[, 1])  ##The only significant modification has to be 
                                #made at this point: a column of "1" is 
                                #added to X 

# Now carry out intermediate calculations
XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%y
beta = invXTX %*% XTy

#Now beta is a two dimensions vector: the first element is the estimation of
#b_0 and the second one of b_1


# Now add this line to the plot
plot(out)
abline(lm(out[,2]~out[,1]), col="red") # regression line (y~x) 
#The second modification is there: since beta has 2 elements now, b_0 is the 
#intercept (so it is the value of a in the abline() function), and b_1 is the 
#slope (b in abline())
abline(a=beta[1], b=beta[2], col="blue")




###########
#Question 3
###########

#In the previous question we tried to estimate in which way x influences y.
#Now we consider that there is another variable that has a normal law. The 
#mean of the 3 variables has to be defined, hence MU is a 1*3 vector.
#Dimension of the variance-covariance SIGMA matrix was previously 2*2; this time 
#it will be 3*3. 
#The values of the covariance parameters (ie non diagonal terms of SIGMA) have to
#be defined. 

#Definition of means of the three variables y, x1, x2
MU <- c(4, 2, 1)

#Definition of covariance parameters: Cov(x_i, x_j) = c_ij if i<>j; 
#c_ij = Var(xi) if i=j. Here we choose all the variances equal to one, and 
#others c_ij are arbitrary choosen.

c12<-0.3
c13<-0.6
c23<-0.9

SIGMA = matrix(c(1, c12, c13, c12, 1, c23, c13, c23, 1), ncol = 3)

#In order to have statistical robustness of estimation, a certain number of 
#observations is required. Ultimate minimum is generally considered as 30; 200 
#is more confortable. 

N <- 200


#Definition of the sample: 
S<- mvrnorm(N, mu = MU, Sigma = SIGMA)


#We want to estimate the coefficients of the regression, for each individual i
#of the sample: 
#y_i = beta_0 + beta_1.x_i1 + beta_2.x_i2 + epsilon_i

#Defining Y as the dependent variable and Xi the explanatory variables. We are 
#careful to define a vector that contains only 1, in order to take into account 
#estimation of beta_0.

Y<-S[, 1]
X<- cbind(rep(1, N), S[, c(2, 3)])
X1<-S[, 2]
X2<-S[, 3]


model<-lm(Y~X1+X2)
summary(model)

model$coefficients



#Computation of the OLS estimators of betas "by hand"


# Now carry out intermediate calculations
XT = t(X)
XTX = XT%*%X
invXTX = solve(XTX)
XTy = XT%*%Y
beta = invXTX %*% XTy
beta

#We check wether estimations of lm() functions and ours are identical: 

mat<-cbind(model$coefficients, beta)
print(mat)

#Watching coefficients, they seem equal.
#However, compiling the following line, R returns that there is no equality. 
#This might be due to slight difference of computations into the lm() function. 
mat[,1]==mat[,2]





###########
#Question 4
###########


#Here we have the choice for the parameters of the function that we call 
#general_OLS(). As it it asked to the function to take arbitrary parameters, 
#we could consider that it computes them itself. However, we let an option 
#if we want to define some parameters.

general_OLS<-function(nvar, #Number of variables
                      MU,   #Vector of means
                      SIGMA,#Variance-covariance matrix
                      N     #Sample size
                      ){
  
  #The following if statements enable parameters not to be given in the call 
  #of the function. If they are not given, a value is totally randomly 
  #attributed. For instance, the total number of variables is between 
  # 2 and 102. 
  
  
  if(is.na(nvar)){
    nvar<-floor(runif(1,0,1)*100+2)
  }
  
  if(is.na(MU)){
    MU<-runif(nvar, 5, 25)
  }
  
  #The definition of arbitrary variance-covariance matrix is a little bit
  #technical, because it has to be symmetrical and positive definite. 
  #To do this, we "just" have to create a random matrix X, and form the 
  #matrix X.t(X), which is positive definite and invertible. 
  
  if(is.na(SIGMA)){
    coeff_sigma<- runif(nvar*nvar, 0, 1)
    SIG<- matrix(coeff_sigma, ncol = nvar, nrow = nvar)
    SIGMA<-SIG%*%t(SIG)
  }
  if(is.na(N)){
    N<-floor(runif(1,0,1)*1000+5*nvar)
  } 
  
  sample<- mvrnorm(N, mu = MU, Sigma = SIGMA)
  
  Y<- sample[, 1]
  X<- cbind(rep(1, N), sample[, c(2:(nvar))])
  XT = t(X)
  XTX = XT%*%X
  invXTX = solve(XTX)
  XTy = XT%*%Y
  beta = invXTX %*% XTy
  return(beta)

  #Plotting the results
  if(nvar == 2){
    graph<-plot(sample)
    abline(lm(sample[,2]~sample[,1]), col="red") 
    abline(a=beta[1], b=beta[2], col="blue")
  }
  #This part is not functional, there is only the idea
  if(nvar == 3){
    library(plot3D)
    graph<-persp3D(x = sample[, 2], y = sample[, 3], z = sample[, 1], colvar = z)
  }
  return(graph)
}

#Example with a predifinite number of variables 
general_OLS(nvar = 2, MU = NA, SIGMA = NA, N = NA)




