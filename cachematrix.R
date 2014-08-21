## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatr <- function(matr) mat <<- matr
  getmatr <- function() mat
  list(set = set, get = get, setmatr = setmatr, getmatr = getmatr)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## If inverse has already been calculated (and matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        mat <- x$getmatr()
        if(!is.null(mat)){
          message("getting cached data")
          return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setmatr(mat)
        mat
}
