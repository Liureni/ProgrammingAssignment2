## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
## set the value of the vector
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## get the value of the vector
  get <- function() x
  
## set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
## get the inverse of the matrix
  getinverse <- function() inv
  
## set and get the value of the mean
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the mean of the "makeCacheMatrix" 
## created above. However, it first checks to see if the 
## mean has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data 
## and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
