## This file contains 2 functions
## makeCacheMatrix() - function that provides getters and setters for the passed in matrix
## and its respective inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Gets the inverse from the cache or else computes the cache 
## and sets it to the variable m

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached Inverse Matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
  }
  



