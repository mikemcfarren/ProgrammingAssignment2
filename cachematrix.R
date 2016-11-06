## Assignment: Caching the Inverse of a Matrixless 

## "Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here)."

## "Computing the inverse of a square matrix can be done with the
## solve function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse."

## makeCacheMatrix: This function creates matrix object that can cache its inverse, 
## assumes the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
    m.inverse <- NULL
    set <- function(y) {
        x <<- y
        m.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m.inverse <<- inverse
    getinverse <- function() m.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function checks to see if matrix has inverse, if
## it does, return the cached object.  Otherwise, invert the matrix, 
## cache the results, and return the inverse.

cacheSolve <- function(x, ...) {
    m.inverse <- x$getinverse()
    if(!is.null(m.inverse)) {
        message("getting cached data")
        return(m.inverse)
    }
    data <- x$get()
    m.inverse <- solve(data, ...)
    x$setinverse(m.inverse)
    m.inverse
}


## create a sample matrix that is invertible

createSampleInvertibleMatrix <- function()
{
    m <- rbind(c(1, -1/10, -1/10, -1/10, -1/10), c(-1/10, 1, -1/10, -1/10, -1/10), c(-1/10, -1/10, 1, -1/10, -1/10), c(-1/10, -1/10, -1/10, 1, -1/10), c(-1/10, -1/10, -1/10, -1/10, 1))
    m
}


## testing

## z <- createSingularMatrix()
## m = makeCacheMatrix(z)
## first time, no cache... generate
## cacheSolve(m) 
## second time, HAS cache... return cache
## cacheSolve(m) 



