## cachematrix.R - R Programming Week 3 Assignment 2
## Assignment: Caching the Inverse of a Matrix
##
## This script contains a pair of functions to cache the inverse of a matrix.
## 
## 1. makeCacheMatrix: Creates a "matrix" object able to cache its inverse.
## 2. cacheSolve: Computes the inverse of the "matrix" object return by {1}.
##    If the inverse has been computed, retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the "matrix" object created with the function
## above. Check if the inverse has been computed already, if present (!null)
## return. If not, solve and setInverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("Cached inverse found.")
          return(inv)
        }
        
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
}