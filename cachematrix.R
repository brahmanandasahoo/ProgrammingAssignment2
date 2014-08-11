## This file contains a set of two function for Caching the Inverse of a Matrix

## The makeCacheMatrix function returns a special "matrix" object 
## The special "matrix" object is a list that can cache its inverse containing 

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

## Input to this method is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        
        invX <- NULL
        
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        
        get <- function() x
        setInv<- function(xInv) invX <<- xInv
        getInv <- function() invX
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix  
## If the inverse has already been cached then cacheSolve retrieve the inverse from the cache
## Else cacheSolve claculates the inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
        
        invMatrix <- x$getInv()
        
        if (!is.null(invMatrix)) {
                message("Getting cached data ...")
                return(invMatrix)
        }
        
        data <-x$get()
        invMatrix <-solve(data , ...)
        x$setInv(invMatrix)
        
        invMatrix
}
