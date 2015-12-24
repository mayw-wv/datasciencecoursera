## Put comments here that give an overall description of what your
## functions do

## This function will create an object that stores a matrix "x"
## along with it's inverse matrix, so that the
## subsequent calls to the related function
## "cachesolve" can simply access the inverse without having
## to recalculate the inverse

## mat <- matrix(c(1,0,0,1,1,0,0,0,1), nrow=3, ncol=3)
## > mat
## [,1] [,2] [,3]
## [1,]    1    1    0
## [2,]    0    1    0
## [3,]    0    0    1
## > matObj <- makeCacheMatrix(mat)
## > matinv <- cacheSolve(matObj)
## > matinv
## [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    0    1    0
## [3,]    0    0    1
## > matinv2 <- cacheSolve(matObj)
## getting cached data

## notice the difference between matinv and matinv2 which involve the same call

makeCacheMatrix <- function(x = matrix()) {
  # "makeCacheMatrix" inputs a matrix and outputs an object that
  # includes the original vector plus some associated functions
  # that set and get the mean of the vector.
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # the next statement checks if the inverse has already been computed
  # if true the saved inverse is returned without recalculating
  # if false then the inverse must be calculated - but it is then stored
  # in case it is needed again
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)      # the previously calculated and saved inverse is returned
  }
  # this is the false section - it is done the first time (only) when
  # the inverse needs to be calculated.  The inverse is then stored in "m"
  # and m is returned by the function cachesolve.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        # finally the calculate mean is returned
}
