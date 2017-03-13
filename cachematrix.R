##############################################################################
#
# Filename: cachmatrix.R
#
# This file defines the functionality for creating a cached matrix and computing
# its inverse matrix by either returning a previously computed vallue or
# computing the inverse matrix for it.
#
# Functions:
#   - makeCacheMatrix
#   - cacheSolve
#
# Date: March 13, 2017
#
# Testing:
# cm <- makeCacheMatrix(matrix(1:4, nrow=2))
# cacheSolve(cm)
## This should return the calculated inverse matrix
# cacheSolve(cm)
## This should return the cached version
#
# cm.2 <- cacheSolve(cm)
# cm$set(cm.2)
# cacheSolve(cm)
## This should return the caculated inverse matrix
#
## Let's try something interesting
# cm <- makeCacheMatrix(matrix(runif(3000^2), 3000))
# cm.2 <- cacheSolve(cm)
## This should return the calculated inverse matrix
# cacheSolve(cm)
## This should return the cached version and take much less time
#
# Thank you for your time.
#
##############################################################################

##############################################################################
#
# Function: makeCacheMatrix
#
# Creates a cache matrix from a regular matrix. The cache matrix will be able
# to set/get data from the matrix and set/get the inverse matrix from a cache.
#
# Args:
#   x: matrix to convert into a cache matrix
#
# Returns:
#   cache matrix
#
##############################################################################

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

##############################################################################
#
# Function: cacheSolve
#
# Computes the inverse of the matrix x if a previously computed inverse of the
# matrix does not exist in its cache. The computed inverse matrix uses solve to
# to compute the inverse matrix of x.
# 
# Args:
#   x: the matrix for which the inverse matrix is to be computed
#
# Returns:
#   inverse matrix of x
#
##############################################################################

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


