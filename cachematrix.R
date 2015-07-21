## makeCacheMatrix creates a special "matrix" object that cache its inverse
## cacheSolve will computes the inverse of the special "matrix" object created
##    by makeCacheMatrix above.  If the inverse was calculated before, the inverse
##    should be retrieved from the cache

## It create a special "matrix" object with functions
##   set - set the matrix
##   get - get the matrix
##   setInv - set the inverse
##   getInv - get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## The following function calculates the inverse of the special "matrix" created with makeCacheMatrix
##   It first checks to see if the inverse is already been calculated.  If so, the cache is returned
##   Otherwise, the inverse is calculated and set the value in the cache via setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached Inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
