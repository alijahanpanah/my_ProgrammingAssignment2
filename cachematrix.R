## I have followed the example to write this function. The main steps:
## set the value of the matrix, get the value of the matrix, set the value of the inverse
## and finally get the value of the inverse.

## The function sets 'inv' to null initially, and the rest follows the example.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## This function also follows the example, so that after first time computation, it will cache it instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

## I tested this R function on a 2x2 matrix as follows:
## Created a matirx by this:   my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))  
## my_matrix$getinverse()  returns NULL value 
##cachesolve(my_matrix) returns (computes) the inverse matrix
## repeating the function will return the inverse matrix, but this time caching it instead. 
## testing on a 3x3 matrix returned an error!
