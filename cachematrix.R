## The following pair of functions does the following:
## 1) makeCacheMatrix creates a list of objects which acts as a cache for the input matrix
## 2) cacheSolve returns inverse if there is no cached inverse, else it calculates the inverse
## This saves time with very large datasets

## This function creates a list of objects which acts as a cache for the input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns inverse if there is no cached inverse, else it calculates the inverse

cacheSolve <- function(x, ...) {
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
