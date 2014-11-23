## Pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    x
  }
  
  ## set the inverse of matrix
  setinverse <- function(inverse) {
    
    i <<- inverse
  }
  
  ## get the inverse of matrix
  getinverse <- function() {
    
    i
  }
  
  ## list of methods, which will be returned
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ## check the cached data
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse
  x$setinverse(m)
  
  ## Return the matrix
  m
}
