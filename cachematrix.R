## This functions have the objective of creating a special matrix and computing
## the value of it's inverse.

## This function creates a special matrix. A list containing the value of the 
## matrix, it's inverse, and the capability of setting those values.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of the special matrix if it's not 
## already stored in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()    ## Return a matrix that is the inverse of 'x'
    if (!is.null(i)){
        print("Obtaining cached data")
        return(i)
    }
    val <- x$get()
    i <- solve(val)
    x$setinv(i)
    i
}
