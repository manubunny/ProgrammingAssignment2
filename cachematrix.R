## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makecacheMatrix function takes input as matrix, this sets"I"-inverse as NULL
## this function returns list of functions as part of the object and declares 
## and defines four functions set,get,setInverse,getInverse for setting data in 
## memory and returning that data back.

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y)
  {
    x <<- y
    
    I <<- NULL
  }
  get<-function()
  {
    x
  }
  
  setInverse <-function(Inverse)
  {
    I <<- Inverse
    
  }
  getInverse <-function()
  {
    I
    
  }
  
  list(set = set, get = get,setInverse = setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## cacheSolve takes matrix created by makeCacheMatrix
## Accesses "I" set in makeCacheMatrix() using getInverse function
## checks for a value in Inverse, if matrix is same as previous matrix for which 
## inverse is set earlierit will retrun the value without calculating again.
## if it is NULL or new matrix is input then it will calculate Inverse again and 
## cache it using setInverse for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  
  
  
  if(!is.null(Inverse))
  {
    message("getting cached data\n")
    return(Inverse)
  }
  
  data <-x$get()
  Inverse <- solve(data)
  x$setInverse(Inverse)
  {
    Inverse
    
  }
  
}
