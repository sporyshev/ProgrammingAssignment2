## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {

  ## Initialize the inverse
  i <- NULL

  ## Function to set the matrix
  set <- function(x) {
    m <<- x
    i <<- NULL
  }

  ## Function to get the matrix
  get <- function() m

  ## Function to set the inverse of the matrix
  setInverse <- function(x) {
    i <<- x
  }

  ## Function to get the inverse of the matrix
  getInverse <- function() i

  ## Return a list of the functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve computes the inverse of the special "matrix".
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Get a matrix that is the inverse of "x"
  i <- x$getInverse()

  ## Check cached inverse
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  ## Get the matrix
  m <- x$get()

  ## Calculate the inverse of the matrix
  i <- solve(m, ...)

  ## Cache the inverse of the matrix
  x$setInverse(i)

  ## Return the inverse of the matrix
  i

}
