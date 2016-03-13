## These functions will create a special matrix object that
## cache its inverse and compute the inverse of the special 
## matrix.

## makeCacheMatrix will create a special matrix object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv, getinv)
}

## This function computes the inverse of the special matrix
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
}