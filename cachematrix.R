makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


##Mine

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix function takes a matrix as input and sets Up getter and Setter
## functions for the data and the Matrix Inverse
## Uses <<- operator for pushing data in the variables to enable Caching
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
## The function takes a Matrix as input , checks if Inverse has been set by
## previous operations using is.Null Check.
## Gets data from saved Cache variable if unchanged matrix is passed again
## Returns Message "Getting from Cache" if retrieved from Cache was successful

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
