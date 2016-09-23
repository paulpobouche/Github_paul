#Homework 2 
#Generalization of gambler's ruin
#Paul Bouche
#20/09/2016

###########
#Question 1 
###########

#In order to loop over different value of probability to win p, there are
#several solutions. I assume that the probability is random and different 
#between each loop. I could have assumed also that p can take a finite number 
#of values but is randomly (or not)  taken between them for each loop, 
#for instance. 



#Initial wealth
w0<-100

#Number of loops
M<-10000

#Creation of the vector containing the value of gambler's wealth at 
#each loop, w1. The first value of this vector is the intial wealth
w1<-rep(0, M+1)
w1[1]<-w0

#Creation of the vector containing the result of the game: win (1) or lose (0)
# at each loop
x<-rep(0, M)


for (i in 1:M){
  p<-runif(1,0,1) #Probability to win, random for each period
  s<-runif(1,0,1) #"Shock" that will determine if the gambler has won or not,
                  #random for each period
  
  if(s<=p){        #in this case the gambler wins
    x[i]<-1       #the ith value of the vector is filled by 1 in case of winning
    w1[i+1]<-w1[i]+1 #the wealth increases by 1
    
  } else {
    x[i]<-0       #the ith value of the vector is filled by 0 in case of loosing
    w1[i+1]<-w1[i]-1 #the wealth decreases by 1
  }
}
  

#It now possible to draw the "history" of gambler's wealth

periods<-c(1:(M+1))
  
plot(x = periods, y = w1, type = "l", col = "blue")
  
###########
#Question 2 
###########

#This time, the shock is fixed and the probability is random. Therefore the shock 
#is defined before the loop. 


#Initial wealth
w0<-100

#Number of loops
M<-10000

#Creation of the vector containing the value of gambler's wealth at 
#each loop, w2. The first value of this vector is the intial wealth
w2<-rep(0, M+1)
w2[1]<-w0

#Creation of the vector containing the result of the game: win (1) or lose (0)
# at each loop
x<-rep(0, M)
s<-runif(1,0,1) 

for (i in 1:M){
  p<-runif(1,0,1) #Probability to win, random for each period

  if(s<=p){        #in this case the gambler wins
    x[i]<-1       #the ith value of the vector is filled by 1 in case of winning
    w2[i+1]<-w2[i]+1 #the wealth increases by 1
    
  } else {
    x[i]<-0       #the ith value of the vector is filled by 0 in case of loosing
    w2[i+1]<-w2[i]-1 #the wealth decreases by 1
  }
}


plot(x = periods, y = w2, type = "l", col = "red")

###########
#Question 3
###########

#In the previous question, the plot was specific to a case. It is also possible 
#draw both evolutions of weath on the same plot. There is a huge variability for
#the second option, for instance if the shock is randomly 0.0002, the gambler
#will have a very small probability to loose (and will concretely win each 
#period). Therefore it is useful to know extremes values of w1 and w2 to 
#adjust the graph so that both curves are well represented.

#The boundaries of y axis are defined by the minimum and the maximum of each 
#vector of wealth
maxval<-max(w1, w2) 
minval<-min(w1, w2)



plot(x = periods, y=w1, col = "blue", type = "l", ylim= c(minval, maxval), 
     main = "Gambler's wealth evolution", 
     xlab = "Number of bets", ylab = "Wealth")
lines(x = periods, y = w2, col = "red", type = "l")
legend("bottomleft", 
       legend = c("Random p and s", "Random p and fixed s"), 
       col= c("blue", "red"), lty = c(1, 1))
#A remark about this legend: the first option gives the position of the 
#legend on the graph. Therefore I choose "bottomleft" because it fits the shapes
#of the curves. Of course if I run again, those ones will probably differ and I 
#will have to change the position (topleft, topright...)


###########
#Question 4
###########

#This time, two loops will be runned. The previous one remains the same, but 
#it is included into another loop, that gives the different realizations
#of the final wealth. This time, as there is only one probability to win, 
#the shock is random. 





#Initial wealth
w0<-100

#Number of loops
M<-10000

#Number of realizations, or trial, i.e. loops of previous. As the computation 
#times grow exponentially, I reduce the number of realization, 100 will be enough
R<-100

#Creation of the vector containing the value of gambler's wealth at 
#each loop, w4. The first value of this vector is the intial wealth. 

#This time, at the end of each loop, I will keep the final wealth and include 
#it into a vector K; at the end I will compute the average of those final wealths
 
K<- c(0, R)


p<-runif(1,0,1) #The probability will always be the same (otherwise the idea 
                #of doing the mean of results has less sens)

for(j in 1:R){  #Beginning of the loops of the trials
  
  w4<-rep(0, M+1) #Vector that will contain all the values of the wealth 
                  #for the jth realization. 
  w4[1]<-w0
  
  for (i in 1:M){ 
    
    s<-runif(1,0,1) 
    
    if(s<=p){            
      w4[i+1]<-w4[i]+1 
      
    } else {
      w4[i+1]<-w4[i]-1  
    }
  } #End of the jth game of the gambler
  
  K[j]<-w4[M+1] #Plugging the final wealth into the jth value of K
  
} #End of the loops of the trials
 
ave<- mean(K) #The object ave contains the mean of final wealths

###########
#Question 5
###########

#This time, I can consider two options. Either the probability varies within each
#trial and is the same for all the loops of this one, either it varies in each 
#loop of each trial. 

#Let's try the first alternative

#Initial wealth
w0<-100

#Number of loops
M<-10000

#Number of realizations, or trial, i.e. loops of previous. As the computation 
#times grow exponentially, I reduce the number of realization, 100 will be enough
R<-100

#Creation of the vector containing the value of gambler's wealth at 
#each loop, w4. The first value of this vector is the intial wealth. 

#This time, at the end of each loop, I will keep the final wealth and include 
#it into a vector K; at the end I will compute the average of those final wealths

K<- c(0, R)

for(j in 1:R){  #Beginning of the loops of the trials
  p<-runif(1,0,1) 
  w4<-rep(0, M+1) #Vector that will contain all the values of the wealth 
                  #for the jth realization. 
  w4[1]<-w0
  
  for (i in 1:M){#Beginning of the ith bet of the gambler
    
    s<-runif(1,0,1) 
    
    if(s<=p){            
      w4[i+1]<-w4[i]+1 
      
    } else {
      w4[i+1]<-w4[i]-1  
    }
  } #End of the ith bet of the gambler
  
  K[j]<-w4[M+1] #Plugging the final wealth into the jth value of K
  
} #End of the loops of the trials

ave2<- mean(K) #The object ave contains the mean of final wealths



###########
#Question 6 
###########































  
  


