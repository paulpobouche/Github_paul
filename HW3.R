#Homework 3
#Paul Bouche
#21/09/16

#Sorry I lacked of time...

###########
#Question 1
###########

#Documentation on functions

############ Comment
# Normally one puts the documentation before the function. Additionally
# try to keep your documentation organized, eg giving standard
# fields to fill such as Author, Title, Description, Inputs., etc.

#Function 1

add<- function(a, b){
  return (a+b)
}

#The function add() has two arguments, which are real numbers ("doubles" to be
#precise). It returns the sum of those two numbers.



#Function 2

whereis <- function(pattern, word){
  return(grep(pattern, word))
}

#The two arguments of the function whereis() are characters. It uses the grep()
#function, that indicates the position of the first argument into the string of
#caracters of the second one. The function returns this position.

#Function 3

our_sum <- function(x){
  summation <- 0
  for(i in x){
    summation <- summation + i
  }
  return(summation)
}

#The function our_sum has only one argument: x, a vector of real numbers (doubles).
#It consists into a loop on the values of this vector, summing each of them.
#Hence it returns the sum of the values of vector's elements.


#Function 4

F_inv_unif <- function(x, a, b) {
  x*(b - a) + a
}



#The function F_inv_univ has three arguments. The last ones, a and b, are real
#numbers. The first one, x, is a vector of realizations of a uniform law on
#interval [a,b].
#Hence, the repartition funciton of each element of x takes the value :
#(x-a)/(b-a).
#Therefore, image of x by F_inv_unif is a set of #x realizations of a
#uniform law on [0,1]



#Function 5

rand_sample <- function(n, F_inv=function(x) x){
  unif_sample <- runif(n)
  return(F_inv(unif_sample))
}

#The function rand_sample has two arguments: an integer n and a function F_inv.
#It returns a set of n realizations of a uniform law on [0,1]
