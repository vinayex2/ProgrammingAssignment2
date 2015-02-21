## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse<-NULL
  set<- function(y)
  {
    x <<- y
    matrixInverse <- NULL
  }
  
  get <- function() x
  setInverse<- function(inverse=matrix())
  {
    matrixInverse <<- inverse
  }
  getInverse <- function() matrixInverse
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("Getting from cache")
    return(inverse)
    
  }
  
  matrix<- x$get()
  inverse<-solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
