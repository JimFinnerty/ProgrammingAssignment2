## Purpose:
##   To provide an optimized invertable square matrix implementation that maintains
##   a cache of the inverse of the matrix.  The caller is responsible for ensuring
##   that the matrix is square and is invertable.  
##
## External Functions:
##   makeCacheMatrix(x = matrix()) - Constructs a list structure that can hold a 
##       square invertable matrix and its inverse.  The inverse is constructed
##       lazily upon request (see getInv)
##   cacheSolve(x) - Calculates, caches, and returns the inverse of the matrix x 
##       if not previously cached.  If previously cached, then the cached value
##       of the matrix inverse is returned.
##
## Private Functions in CacheMatrix:
##   set(y) - sets y as the square invertable matrix, x
##   get()  - returns the square invertable matrix, x
##   setInv(inv) - sets the inverse of the matrix to inv
##   getInv() - returns the cached inverse of the matrix
##

## Construct a CacheMatrix from the square invertable matrix x.  It is the callers'
## responsibility to ensure that x is square and invertable.

makeCacheMatrix <- function(x = matrix()) {
  invCache <- NULL
  set <- function(y) {
    x <<- y
    invCache <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invCache <<- inv
  getInv <- function() invCache
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Returns the inverse of the invertable square matrix x and caches the inverse in
## the CacheMatrix.  If previously cached, the cached inverse is returned without
## recalculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invCache <- x$getInv()
  if(!is.null(invCache)) {
    message("getting cached data")
    return(invCache)
  }
  data <- x$get()
  invCache <- solve(data, ...)
  x$setInv(invCache)
  invCache
}
