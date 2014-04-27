## Johan Ahlberg 2014-04-27

## The functions below are used for inverting a matrix and keeping
## the inverse in memory, such that it only has to be computed once

## makeCacheMatrix stores a matrix and its inverse. It is initiated
## with a numeric matrix as argument.

makeCacheMatrix <- function(x = matrix()) {
  x <- x
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a "makeCacheMatrix" as argument and
## returns its inverse. If the inverse was not cached before
## it is computed and stored in memory.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
  } else {
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
  }
  return(inv)
}

