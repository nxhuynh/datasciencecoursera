## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a special matrix obj that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function(){ x }
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    getInverse <- function(){
        ret <- NULL
        if (nrow(x) != ncol(x))
            message("Non-squared matrix can't be inversed")
        else
            ret <- i
        ret
    }
    setInverse <- function(ii){ i <<- ii }
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function
## returns the inverse of the input matrix x
## if the inverse was computed, returned the cached results
## otherwise, compute the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (is.null(i)){
        i <- solve(x$get(), ...)
        x$setInverse(i)
    }
    i
}
