## Matrix inversion is a typically costly conversion and there may be some
## benefit to caching the inverse of a matrix rather than repeatedly
## computing it.
## The functions makeCacheMatrix and cacheSolve below are used to cache 
## the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function below returns the inverse of the matrix. If the inverse has
## already been computed, it gets the result and skips the computation. 
## If it has not already been computed, it computes the inverse and sets the value
## in the cache through the setInverse method.

## Assuming the matrix is always invertible:
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
