## The following functions are designed to create a structure holding a matrix
## and to query it in an efficient way i.e. using cache


## creates a structure holding the matrix
## the matrix has methods get and set to read from/write to the matrix itself
## as well as getInv and setInv to read from/write to the cache holding the inverse
## of that matrix
## updates of the matrix always clear the cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInv <- function() inv
  setInv <- function(i) inv <<- i
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}

## queries a structure created using makeCacheMatrix for its inverse
## if the cache is available, its value is returned
## otherwise the new inverse is computed, put into the cache
## and returned
cacheSolve <- function(x, ...) {
  if(is.null(x$getInv())) {
    inverse <- solve(x$get(), ...)
    x$setInv(inverse)
    inverse
  } else {
    message("Returning the cached value")
    x$getInv()
  }
}
