## The MakeCacheMatrix function set the matrix and the inverse in an enviroment
## Where x is an invertible matrix 

## Examples
## x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
## x$set(matrix(1:4), 2, 2)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function(x) {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute and cache the inverse of a matrix
## where x is an inverse matrix thats already been passed through makeCacheMatrix

## Example
##  x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
## cacheSolve(x)

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv))
  mat <- x$get()
  inv <- solve(mat, ...)
  x$getInverse(inv)
  inv
        
}
