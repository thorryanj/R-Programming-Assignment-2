## Put comments here that give an overall description of what your
## functions do

# Due to Matrix inversion typically being a costly computation, there may be benefit
# to caching the inverse of a matrix rather than having to compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data.")
        return(invs)
    }
    mtx <- x$get()
    invs <- solve(mtx)
    x$setinverse(invs)
    invs
}