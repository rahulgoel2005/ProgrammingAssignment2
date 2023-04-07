## Caching of matrix inverse is achieved through two functions (1) makeCacheMatrix and 
## (2) cacheSolve, which work together.

## makeCacheMatrix function creates special function attributes for the matrix which
## enable access to matrix data, and set and get matrix inverse value when called by 
## cacheSolve. Lexical scoping and "<<-"super-assignment operator are used to modify 
## initialized value of the matrix inverse.

## cacheSolve function calculates the value of matrix inverse and uses the special
## attributes to set inverse value the first time when cacheSolve is invoked. 
## After the inverse value has been  set, it retrieves the stored inverse value 
## each subsequent time cacheSolve is invoked.

## Example of how to use:
## a_matrix <- matrix(c(4,7,2,6), nrow=2, ncol=2, byrow=TRUE) ## a matrix
## A <- makeCacheMatrix(a_matrix) ## transforms a matrix to makeCashMatrix object
## cacheSolve(A) ## returns cached inverse of a_matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(get = get,setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
# x is a makeCaseMatrix type object
        
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


