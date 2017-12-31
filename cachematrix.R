##################################################################################
## The cache the inverse of a matrix                                            ##
## -----------------------------------------------------------------------------##
## The following programme writes a pair of functions that cache the            ##
## inverse of a matrix                                                          ##
##################################################################################

## This function creates a special "matrix" object that can cache its inverse   ##

makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y){
        x <<- y
        inversematrix <<- NULL
    }
    get <- function()x
    setinv <- function(inv) inversematrix<<-inv
    getinv <- function()inversematrix
    list(set = set, get=get,
         setinv=setinv,
         getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversematrix <- x$getinv()
    if(!is.null(inversematrix)){
        message("getting cached data")
        return(inversematrix)
    }
    data <- x$get()
    inversematrix <- solve(data,...)
    x$setinv(inversematrix)
    inversematrix
}
