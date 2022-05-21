@@ -1,15 +1,37 @@
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## makeCacheMatrix creates a special “matrix” object that can cache its inverse
## cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix above
## assume that the matrix supplied is always invertible

##The following functions are used to create a special object that stores a matrix and caches its inverse
##this function sets, gets the elements of the matrix, 
##then sets, gets the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function computes the inverse of the special matrix returned by makeCacheMatrix above
##calculates the inverse of the matrix created in the preceding function;
##checks to see if the inverse has been calculated above; if yes then gets the inverse from cache;
##if no, performs calculation, then sets cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
