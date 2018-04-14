## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will create a special object that can cache its inverse
## cacheSolve will compute the inverse of the special matrix returned by makeCacheMatrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  set <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv1 <<- inverse
  getInverse <- function() inv1
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## calculates the inverse of the matrix returned by makeCacheMatrix
## checks if the matrix is same and inverse is already calculated
## if yes, it retrieves the cached value,else it calculates and returns the inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv1 <- x$getInverse()
  if (!is.null(inv1)) {
    message("getting cached data")
    return(inv1)
  }
  mat <- x$get()
  inv1 <- solve(mat, ...)
  x$setInverse(inv1)
  inv1
}
