## Short Summary: This R file contains a pair of functions allowing to cache and
## reuse the inverse of a matrix.

## Motivation: Taking the inverse of a matrix may take a significant time, 
## especially if it has to be computed repeatedly (e.g. in a loop). If the 
## contents of a matrix are not changing, it may make sense to cache the value 
## of the inverse matrix so that when we need it again, it can be looked up in 
## the cache rather than recomputed. This is implemented by the two following 
## functions.


## How to test these functions:
## Create an empty CacheMatrix object:
# > cm <- makeCacheMatrix()

## Test cm$set()
# > cm$set(matrix(c(1, 2, 3, 4), 2, 2))

## Test cm$get()
# > cm$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## Test cacheSolve() [first time --> it has to cumpute inverse]
# > cacheSolve(cm)
# Computing new inverse matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## Test cacheSolve() [second time --> inverse already computed]
# > cacheSolve(cm)
# Returning cached inverse matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



## makeCacheMatrix() --> This function creates a special object that contains a
## matrix and can cache its inverse.

## The object returned is a list containing 4 functions, and the context in 
## which the list is defined hosts the matrix ("x" variable) and its inverse 
## ("i" variable). This function does not actually compute te inverse: this has
## to be done by explicitly calling the cacheSolve() function on the object
## returned by makeCacheMatrix().

## The purpose of the 4 functions contained inside the list that
## makeCacheMatrix() returns is the following:

## set() --> store the matrix in the list context
## get() --> get the matrix from the list context 
## setinv() --> set the value of the inverse matrix
## getinv() --> get the value of the inverse matrix
## The last two methods are intended to be called only form cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # Stores the inverse. If NULL, the inverse must be computed
    
    set <- function(y) {
        x <<- y
        i <<- NULL  # Each time we update the matrix, we reset the inverse
    }
    
    get <- function() {
        x
    }
    
    ## Warning: does not compute the inverse. This function is intended to be
    ## used only by cacheSolve()
    setinv <- function(inv) {
        i <<- inv
    }
    
    ## Warning: may return NULL. This function is intended to be used only by
    ## cacheSolve()
    getinv <- function() {
        i
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes and returns the inverse of the matrix stored in the
## special object returned by makeCacheMatrix() above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## cachesolve() retrieves the inverse from the cache.

## The first parameter of this funtion must be an object returned by 
## makeCacheMatrix(). If more parameters are specified, they are simply passed 
## to the solve() function call that computes the inverse matrix, but they are 
## not used by cacheSolve() itself.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("Returning cached inverse matrix")
        return(i)
        
    } else {
        message("Computing new inverse matrix")
        data <- x$get()
        i <- solve(data, ...)  # Passes any parameter to solve
        x$setinv(i)
        return(i)
    }
}
