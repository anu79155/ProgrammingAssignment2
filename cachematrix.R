## There are two functions makeCacheMatrix & cacheSolve
## MakeCacheMatrix consists of set,get,setinv,getinv

## Caluclating the inverse of matrix using set,get,setinv and getinv functions
## library(MASS) is used to calculate inverse for non squared as welll as squared matrices
## cacheSolve function helps to store inverse of matrix where it can be cached directly
library(MASS)
makeCacheMatrix<-function(x = matrix()) 
{
 inv<-NULL        # initialize inverse value as NULL
 set<-function(y)
   {
   x<<-y
   inv<<- NULL
 }
 
  get<-function()x # function to get matrix x
  
  setinv<-function(inverse) inv<<-inverse
  getinv<-function(){
    
    inver<-ginv(x)
    inver%*%x##function to obtain inverse of a function  
  }
    
 list(set=set,get=get,setinv=setinv,getinv=getinv)
 
}

## cacheSolve function helps to get the cache data 
 ## returning the inverse of the matrix 

cacheSolve <- function(x, ...)
  {
   ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
   
  if(!is.null(inv))
  {
    message("GETTING CACHED DATA!!!")
    return(inv)   #returns inverse value 
  }
  
  data<-x$get()
  inv<-solve(data,...)# Calculating the inverse value
  
  x$setinv(inv)
  inv ## returns the matrix  that is inverse of 'x'
}


