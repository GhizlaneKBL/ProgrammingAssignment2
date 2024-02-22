## Caching the inverse of a matrix:

## The first function, "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Define function to set the value of the matrix.
  set <- function(y) {
    x <<- y    
    m <<- NULL 
  }
  ## Define function to get the value of the matrix
  get <- function() x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Return inverse of matrix x:

## The following function calculates the inverse of the special "matrix" created with
## the above function "makeCacheMatrix". However, it first checks to see if the inverse 
## has been calculated. If the inverse has already been calculated, then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- Solve(data)
  x$setInverse(m)
  m
}
