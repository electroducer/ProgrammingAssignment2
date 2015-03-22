## makeCacheMatrix, cacheSolve
## This file defines two functions that allow for the
## computation of matrix inversion using a cache

## Stores a matrix and its inverse in a cache

makeCacheMatrix <- function(x = matrix()) {
  ## Create a placeholder for the inverse
  inv <- NULL
  ## Create a set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Create a get function
  get <- function() x
  ## Create a setinv function
  setInv <- function(inverse) inv <<- inverse
  ## Create a getinv function
  getInv <- function() inv
  
  ## Return a list of functions for use
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## Returns inverse if already calculated,
## otherwise calculates inverse of stored matrix

cacheSolve <- function(x, ...) {
  ## Check to see if cached inv exists
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  ## Otherwise, get the matrix and compute the inverse
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
