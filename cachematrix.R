## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse.matrix <<- inv
  
  getinverse <- function() inverse.matrix
  
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function

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

