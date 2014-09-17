## The function consist of a pair of subfunction aimed to cache 
## the inverse of a matrix and retrieve back the inverse from the
## cache when needed. 

## The subfunction makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(l = matrix()) {
        t <- NULL ###setting the values of t and n veriables to
        n <- NULL ###a default value (NULL)
        setmtr <- function(n) {
                l <<- n ###caching the input value of the matrix l
                t <<- NULL ###resetting the t value to NULL
        }
        getmtr <- function() l ###returning the matrix l
        setinv <- function(solve) t <<- solve ###setting the inverse matrix t to solve
        getinv <- function() t ###returning the matrix t
        list(setmtr = setmtr, getmtr = getmtr, ###creating the list of the above functions
             setinv = setinv,
             getinv = getinv)
}

## This subfunction computes the inverse of the special "matrix"
## returned by subfunction above

cacheSolve <- function(l, ...) {
        t <- l$getinv() ###getting the value of the matrix l through the inverse matrix t
        if(!is.null(t)) { ###checking the t value 
                message("getting cached data")
                return(t)
        }
        mtr <- l$getmtr() ###getting the value of the input matrix l
        t <- solve(mtr, ...) ### computing the value of the inverse of the matrix l
        l$setinv(t) ###caching the inverse by the setinv function
        t ###returning the inverse matrix t
}
