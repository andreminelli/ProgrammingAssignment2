## Functions used to return the inverse of a matrix,
## caching the result to avoid repeated computing

## Creates a special 'matrix' object using a list, which can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## Computes de inverse of special 'matrix' created with 
## makeCacheMatrix, returning a cached value if it
## was previously calculated
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("(Getting inverse matrix from cache)")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
