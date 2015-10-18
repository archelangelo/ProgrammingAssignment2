## Put comments here that give an overall description of what your
## functions do

## Creating a list with functions defined in an environment
## where data are saved.

makeCacheMatrix <- function(x = matrix()) {
  ivr <- NULL
  
  set <- function(y) {
    x <<- y
    ivr <<- NULL
  }
  
  get <- function() x
  
  inv <- function() ivr
  
  setinv <- function(v) ivr <<- v
  
  list(set = set, get = get, setinv = setinv, inv = inv)
}


## Get the cached inverse or calculate it and chache the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivr <- x$inv()
  if (!is.null(ivr)) {
    message("getting cached data.")
    return(ivr)
  }
  ivr <- solve(x$get(), ...)
  x$setinv(ivr)
  ivr
}
