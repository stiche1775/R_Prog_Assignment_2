#==============================================================================
#TITLE           : cachematrix.R
#DESCRIPTION     : R Programing - Course Project 2
#         Write the following functions:
#         1- makeCacheMatrix: This function creates a 
#            "matrix" object that can cache its inverse. 
#         2- cacheSolve: This function computes the inverse of the special
#            "matrix" returned by makeCacheMatrix above. If the inverse has
#            already been calculated (and the matrix has not changed), 
#            then the cachesolve should retrieve the inverse from the cache.
#AUTHOR          : Michael Austin
#DATE            : 2/21/2016
#VERSION         : 0.1
#NOTES           : Script can be executed in R console
#R_VERSION       : R version 3.2.3
#==============================================================================

## assume that the matrix supplied is always invertible.

## 1- makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse, 
## and contains a list containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  inv <- NULL
  
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##         2- cacheSolve: This function computes the inverse of the special
##            "matrix" returned by makeCacheMatrix above. If the inverse has
##            already been calculated (and the matrix has not changed), 
##            then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}