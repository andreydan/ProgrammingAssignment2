## The function consist of a pair of subfunction aimed to cache 
## the inverse of a matrix and retrieve back the inverse from the
## cache when needed. 

## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(l = matrix()) {
        t <- NULL
        n <- NULL
        setmtr <- function(n) {
                l <<- n
                t <<- NULL
        }
        getmtr <- function() l
        setinv <- function(solve) t <<- solve
        getinv <- function() t
        list(setmtr = setmtr, getmtr = getmtr,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix"
## returned by subfunction above

cacheSolve <- function(l, ...) {
        t <- l$getmtr()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        mtr <- l$getmtr()
        t <- solve(mtr, ...)
        l$setmtr(t)
        t
}
