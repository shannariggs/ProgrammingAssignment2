## makeCacheMatrix will calculate the inverse of the matrix, and cache
## the inverse of the matrix
## cacheSolve will retrieve the inverse of the matrix from the cache if
## available, otherwise it will calcuate the inverse of the matrix

## makeCacheMatrix calcuates the inverse of the matrix, and creates a 
## cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix created above. If the 
## cache of the inverse already exists, it will use that data. Otherwise
## it will calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  mat1 <- x$get()
  inv <- solve(mat1, ...)
  x$setInverse(inv)
  inv
}