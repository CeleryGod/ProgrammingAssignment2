## makeCacheMatrix creates a matrix object that can cache it's inverse


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y #sets in different environment
    inv <<- NULL #can't use inverse since it seems to be reserved
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## Checks if already computed, and retrieves from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Trying to save time. Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #get the inverse
  x$setinverse(inv)
  inv #R will return this
}
