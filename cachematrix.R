## Put comments here that give an overall description of what your
## functions do

## Converts matrix x into a "special matrix"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(get = get,setinv = setinv, getinv = getinv)

}


## Computes and sets inverse of matrix x first time, and 
## looks up its value when called again

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

