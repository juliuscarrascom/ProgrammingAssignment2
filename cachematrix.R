## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inversed matrix
## 4. Get the value of the inversed matrix
##
## Then, the second one will attempt to retrieve the cached inverse or
## compute a new inverse matrix instead.

## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above, retrieving the inversed matrix if the
## inverse has already been calculated (and if the matrix has not changed)

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
