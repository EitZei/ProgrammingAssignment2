## Helper functions for creating a cached version of matrix inverse calculation.

## Creates a cache of a given matrix and its inverse.
## Returns a list with following members
## - set (to set the underlying matrix NOTE also clears inverse)
## - get (to get the underlying matrix)
## - setInv (to set inverse of the underlying matrix)
## - getInv (to set the inverse of the underlying matrix)
makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


## Get the the cached matrix inverse or calculate and store the
## inverse to the cache. Pipes the inversing to the solve(x$get(), ...) function.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  ## If the inverse is not yet cached calculate and cache it.
  if (is.null(inv)) {
    message("Caching inverse matrix")
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
  }
    
  return(inv)
}
