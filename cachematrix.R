## This function takes a matrix as an argument and returns an object consisting of a list of functions to cache its 
## inverse.

makeCacheMatrix <- function(x = matrix) {
  
  ## Create variable to store the inverse
  inv <- NULL
  
  ## Create function to store the matrix in variable x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Create function to retrieve the matrix stored in x
  get <- function() x
  
  ## Create function to set the inverse of a matrix
  setSolve <- function(solve) inv <<- solve
  
  ## Create function to retrieve the inverse of a matrix
  getSolve <- function() inv
  
  ## Create list of the functions above
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}

## This function takes as an argument the object returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Retrieve the inverse of the matrix
  inv <- x$getsolve()
  
  ## If the inverse has been calculated (inv is not NULL), retrieve it from the cache and show a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix created by makeCacheMatrix and store it in "data"
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  inv <- solve(data, ...)
  
  ## Store inverse in cache
  x$setsolve(inv)
  
  ##Return inverse
  inv
}