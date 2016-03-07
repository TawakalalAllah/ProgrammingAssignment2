## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## get/set the matrix, and get/set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
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


## Return a matrix that is the inverse of 'x'
## Either by getting cached data, or computing the inverse
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
