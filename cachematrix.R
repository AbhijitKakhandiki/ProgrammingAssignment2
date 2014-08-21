## Test the caching functionality on matrix inversion using the following example in R

## > mat <- matrix(1:4,2,2)   # make a matrix

## > solve(mat)         # check it has an inverse (not required if you're sure)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > matrixObject <- makeCacheMatrix(mat)  # run function to create an object to hold the cached data
## > 
## > cacheSolve(matrixObject)      # this checks to see if the result is cached; if not, it caches it
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
## > cacheSolve(matrixObject)    # 2nd access so now we just fetch cached data
##   getting cached data           #  this is printed when the data is in the cache
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

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
