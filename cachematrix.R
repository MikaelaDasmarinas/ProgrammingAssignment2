## by Mikaela Dasmarinas

## This script writes a pair of functions that cache the inverse of a matrix

## The makeCacheMatrix is a function that creates a special "matrix" 
## object that can cache its inverse. This first function, makeCacheMatrix 
## creates a special "matrix", which is really a list containing a function
## to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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

## This cacheSolve is a function that computes the inverse of the special "matrix"
## returned by the makeCacheMatrix. If the inverse is already calculated, it gets 
## the inverse from the cache and skips the computation.
## If not, it calculates the inverse of the matrix and sets the computed inverse
## as the value of the inverse in the cache via the setinverse function.

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
