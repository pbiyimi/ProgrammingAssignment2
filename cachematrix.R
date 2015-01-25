## Here is my code for the Programming Assignment 2 :
## There are 2 function defined in this file makeCacheMatrix and cacheSolve
## the first function compute the special vector and the second compute the
## inverse. The code this practically the same that the one proposed in the
## assignement page


## This function calculate the "vector" containing the value (matrix) and the
## inverse.
## Input : x the matrix x wanted to be inversed
## Output : a list of 4 functions : setMatrix, getMatrix, setInverse, getInverse
##          setting the value, returning the value, setting the inverse and
##          getting the inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## Input : x, the special "matrix"
##         ..., the option list for the solve function
## Output : the cached inverse if it has already been calculated, computes the
## inverse otherwise

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    # return the cache value
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
