## The following two functions are used to cache the inverse of a matrix, as matrix computation
## is usually costly.

## makeCacheMatrix creates a list containing a function to perform several operations with the matrix
## It sets the value of the matrix, then gets the value of the matrix.
## It sets the value of inverse of the matrix, then gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the special matrix, that was created with the first
## function.
## It first checks whether the inverse was calculated. If not, the cached date is retrieved.
## Then the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieve cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
