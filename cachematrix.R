## Computing the inverse of a matrix is a time-consuming process. 
## These functions save computational time by caching the inverse of a matrix and
## returning the value when needed.

## This function caches the inverse of a matrix in the matrix's object

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(b){
        x <<- b
        a <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) a <<- solve
    getmatrix <- function() a
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function takes the output of the makeCacheMatrix function and 
## computes the inverse of the output. If the inverse has been cached in a previous run,
## and if has not changed, it simply returns the cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    g <- x$getmatrix()
    if(!is.null(g)) {
        message("getting cached data")
        return(g)
    }
    data <- x$get()
    g <- solve(data, ...)
    x$setmatrix(g)
    g
}


