#Hello World
#Paul Bouche, 14/09/2016

#There are several ways to print "Hello world!"

#The first option is to use the basic R function "print"

print("Hello World !")


#However there is also a more complete way to do it, thanks to a function which 
#prints every chain of 3 chains of characters that I want


fun<-function(a, b, char){
  m<- paste(a,  b, char)
  return(print(m))
}

fun("Hello","World", "!")




