## The two functions "makeCacheMatrix" and "cacheSolve" create an object that stores a numeric matrix
## and caches its inverse

## Function makeCacheMatrix returns the following list of functions for matrix x
## [1]: set(y) - set the matrix
## [2]: get()  - get the matrix
## [3]: setinverse(inverse) - set the inverse matrix for later reuse
## [4]: getinverse() - get the cached inverse of the matrix x

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


## Function cacheSolve returns the inverse of the matrix x.
## It returns the cached inverse if available.

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
