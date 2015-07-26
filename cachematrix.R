## Use functions writed below together
## in order to cache computations of matrix inversing
##

## This function store 2 matrices: an original matrix x and inverse of a matrix x.
## This function DO NOT calculate inverse of a matrix - only store.
makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(sol) sol <<- sol
    getsolve <- function() sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function return a matrix that is the inverse of original
## Argument x must be result of the makeCacheMatrix function.
## This function either compute inverse of a matrix 
## or get result from cache (if inverse computed earlier)

cacheSolve <- function(x, ...) {
    sol<- x$getsolve()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(sol)
    sol
}
