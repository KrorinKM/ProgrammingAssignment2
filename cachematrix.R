## The following pair of functions allows to calculate the inverse of a matrix and store it in the cache. If the inverse 
## has already been calculated, it will retrieve it from the cache instead of calculating it again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}