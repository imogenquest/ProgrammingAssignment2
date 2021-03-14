## makeCacheMatrix function
## and cacheSolve function
## together these will 1) create a special "matrix" object
## that can cache its inverse, and 2) compute the inverse
## of the matrix object or, if already calculated and the
## matrix has not changed, it will retrieve the inverse from
## the cache. 

## makeCacheMatrix
## produces a list of functions to
## set the values of the vector
## get the values of the vector
## set the values of the inverse
## get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
## returns the inverse of the matrix x by either
## returning the inv as calculated in makeCacheMatrix
## or if that is null, calculating the inverse and
## setting it as inv in the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
