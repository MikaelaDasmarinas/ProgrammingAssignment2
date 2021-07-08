## by Mikaela Dasmarinas

## This script writes a pair of functions that cache the inverse of a matrix

## makeCacheMatrix is a function that generates a special matrix 
## object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", that is a list containing a function
## to
## "set the value of the matrix"
## "get the value of the matrix"
## "set the value of the inverse"
## "get the value of the inverse"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function that computes the inverse of the special matrix that is
## returned by the makeCacheMatrix. The computation will be skipped if the inverse  
## is already calculated and will only take the inverse in the cache.
## If not, the inverse will be calculated and makes the computed inverse
## as the value of the inverse in the cache by means of the setinverse function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
