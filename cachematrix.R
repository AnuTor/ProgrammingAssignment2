## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Calculates the inverse of the matrix if it's not already stored in the cache

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
