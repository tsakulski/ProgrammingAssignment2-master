## Task: To Cache the inverse of a matrix:
## This function creates a matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##############################################################################

## This function will return the inverse of the matrix created by
## the function "makeCacheMatrix" (written above).
## If the inverse has already been calculated function will obtain
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## This function will return a matrix which is an inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Obtaining cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
