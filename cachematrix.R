
## Below you will find a pair of functions that cache the inverse of a matrix.



##The first function  creates a special matrix object that can cache its inverse.

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



## The second function computes the inverse of the special matrix returned by makeCacheMatrix.


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

#testing

m <- matrix(c(2,4,6,8),2,2)
M1 <- makeCacheMatrix(m)



        ## Return a matrix that is the inverse of 'x'
cacheSolve(M1)
