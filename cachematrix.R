## by Mikaela Dasmarinas

## This script writes a pair of functions that cache the inverse of a matrix

## makeCacheMatrix is a function that generates a special matrix 
## object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", that is a list containing a function
## to
## "**set the value of the matrix"
## "**get the value of the matrix"
## "**set the value of the inverse"
## "**get the value of the inverse"

makeCacheMatrix <- function(zz_input = matrix()) {
  ww_nvi <- NULL
  set <- function(y) {
    zz_input <<- y
    ww_nvi <<- NULL
  }
  get <- function() {zz_input}
  setinv <- function(inverse) {ww_nvi <<- inverse}
  getinv <- function() {ww_nvi}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function that computes the inverse of the special matrix that is
## returned by the makeCacheMatrix. The computation will be skipped if the inverse  
## is already calculated and will only take the inverse in the cache.
## If not, the inverse will be calculated and makes the computed inverse
## as the value of the inverse in the cache by means of the setinverse function.

cacheSolve <- function(zz_input, ...) {
  ## Returns a matrix that is the inverse of 'zz_input'
  ww_nvi <- x$getinv()
  if(!is.null(ww_nvi)) {
    message("getting cached data")
    return(ww_nvi)
  }
  data <- x$get()
  ww_nvi <- solve(data)
  x$setinv(ww_nvi)
  ww_nvi
}
