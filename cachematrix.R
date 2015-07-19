## This file contains Coursera R programming course programming
## assignment 2
## It contains functions to calculate the inverse of a matrix with
## "cache" via lexical scoping.  If the inverse is not calculated
## before, it will use the solve() function to calcuate inverse of x.
## If already calculated previously, it simply returns the "cached"
## inverse of matrix x.

## The makeCacheMatrix is used to build a list containing functions to
## 1. Set the value of the matrix
## 2. Get the vaule of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverse <- function(iX) inverseX <<- iX
  getinverse <- function() inverseX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is used to calculate the inverse of matrix X.
## To use this function:
## 1. cacheX <- makeCacheMatrix(x) to generate a list of functions 
##                 for cacheX.
## 2. cacheSolve(cacheX) will then caculate the inverse of X
## 3. If you call cacheSolve(cacheX) again, it will not call solve;
##    It will simply returned the previous calculated inverse(x)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseX <- x$getinverse()
  if(!is.null(inverseX)) {
    message("getting cached data")
    return(inverseX)
  }
  data <- x$get()
  inverseX <- solve(data, ...)
  x$setinverse(inverseX)
  inverseX
}
