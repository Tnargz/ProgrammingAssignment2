## Week 3 Assignment 2 Lexical Scoping
## cachematrix.R -- Tnargz
## The two functions below will create an object that
## can store a matrix and cache its inverse.


## The following function will create a matrix that can 
## cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function below runs the inverse of the matrix
## created with "makeCacheMatrix". The function will
## return the inverse from the cache if it has already
## been ran or stored.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if (!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setinverse(invMatrix)
  invMatrix
}

## Return a matrix that is the inverse of 'x'
Test1 <- makeCacheMatrix(matrix(1:4, 2, 2))
Test1$get()
Test1$getinverse()
cacheSolve(Test1)
Test1$getinverse()

Test2 <- makeCacheMatrix(matrix(c(1, 3, 6, 9), 2, 2))
Test2$get()
Test2$getinverse()
cacheSolve(Test2)
Test2$getinverse()
