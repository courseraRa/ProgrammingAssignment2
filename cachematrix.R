
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is a matrix of size NxN 
  
  ## Set the value of the mean (m) and the value of the matrix (x)
  m <- NULL  #
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Attributes of the object. 
  get <- function() x  # Get the x object 
  setInverse <- function(solve) m <<- solve  # Set the result of the calculation of the inverse
  getInverse <- function() m # Get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## m is the inverse of the matrix x
  
  ## Check if the inverse of m had been calculate or not, previously. 
  ## If it had been already calculated, return the value of the inverse fron the attribute of the object.
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If the inverse of x had not been already calculated, the function calculates the inverse
  ## and save the result in the attribute of the object x.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}