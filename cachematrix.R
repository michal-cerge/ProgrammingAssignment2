## Functions that cache inverse of a matrix

## First, function that creates special matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
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

## Function that calculates inverse of the special matrix, if the inverse was already calculated
## then it skips the computation and gets the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
