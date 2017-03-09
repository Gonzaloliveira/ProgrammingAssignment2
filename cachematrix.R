## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below creates an invertible matrix.

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Now, this function computes the inverse of the matrix which had been previously
## calculated with the last function.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setInverse(inverse)
  inverse
}

## Whenever you introduce values to the function , the cacheSolve function 
## Calculates the matrix´s inverse.