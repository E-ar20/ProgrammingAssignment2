## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inve <<- inverse}
  getInverse <- function() {inve}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("getting cached matrix inverse")
    return(inve)
  }
  matr <- x$get()
  inve <- solve(matr, ...)
  x$setInverse(inve)
  inve ## Return a matrix that is the inverse of 'x'
}
