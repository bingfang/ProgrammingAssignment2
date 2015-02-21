## The following functions create a special object that stores a matrix and caches the inverse matrix of the matrix.
##
## The makeCacheMatrix function creates a list containing a function to set the Matrix,
## get the matrix,set the inverse of the matrix, and get the inverse of the matrix.
##

makeCacheMatrix <-function(x,...) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  setinverse <- function(im) { inverse <<- im }
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function computes the inverse of the special matrix created 
## with the makeCacheMatrix function. However, it first checks to see if the inverse 
## has already been computeded. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it computes the data and sets the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
