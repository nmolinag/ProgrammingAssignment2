## Two functions are used to create an object that stores a
## matrix and cache its inverse.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list of functions to
## 1.  set the value of the matrix ($set())
## 2.  get the value of the matrix ($get())
## 3.  set the value of the inverse ($setinv())
## 4.  get the value of the inverse ($getinv())

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) matinv <<- solve
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the "matrix"
## created with the above function. It first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the $setinv()
## function.

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv      
  ## Return a matrix that is the inverse of 'x'
}
