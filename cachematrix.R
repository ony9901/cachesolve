## The following function creates a special "matrix",
## which is a list containing a function to:
## > set the value of the matrix
## > get the value of the matrix
## > set the value of the inverse
## > get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # return a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the special "matrix" 
## created with the "makeCacheMatrix" function. 
## This function first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inversein the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
  ## check if the inverse of 'x' is already cached
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse of matrix")
    return(m)
  }
  # If not cached, so get the matrix into data
  data <- x$get()
  # compute the inverse
  m <- solve(data, ...)
  # cache the inverse
  x$setinv(m)
  # return the inverse
  m
}
