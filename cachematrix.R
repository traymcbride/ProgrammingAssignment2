#Programming Assignment 2
#
#Caching the inverse of a matrix.
#
#We can assume for this assignment that the square matrix supplied 
#is invertible.
#
#This file consists of the functions:
#
#1. makeCacheMatrix: This function creates a special "matrix" object that 
#   can cache its inverse.  This object is really a list consisting of 
#   the set, get, setinv, and getinv functions.  Note that x is the matrix
#   passed to makeCacheMatrix.  If nothing is passed, the default is the 
#   1 x 1 matrix with the entry NA.
#
#2. cacheSolve: This function computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix above. If the inverse has already been 
#   calculated (and the matrix has not changed), then cachesolve 
#   retrieves the inverse from the cache.
#
#Command line execution example:
#
#> source("cachematrix.R")
#> my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
#> cacheSolve(my_matrix)
#Populating cache.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


## Pass a matrix to makeCacheMatrix, so that it can create the "matrix" object.
## For example, my_matrix <- makeCacheMatrix(x = matrix(1:4,2,2)).  The value 
## of the matrix passed can be changed using set, for example
## my_matrix$set(matrix(9:12,2,2)).

makeCacheMatrix <- function(x = matrix()) {
    
    #Initialize the inverse to NULL
    
    inv <- NULL
    
    #Set up the get and set functions.  The value of x passed be retained 
    #in the closure of makeCacheMatrix.
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    #Return the list of functions.
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Call cacheSolve with the "matrix" object created by makeCacheMatrix,
## for example cacheSolve(my_matrix).  The inverse of the matrix that was
## passed to makeCacheMatrix will be returned.

cacheSolve <- function(x, ...) {
    
    #Get the inverse matrix.
    
    inv <- x$getinv()
    
    #If inv is not NULL, then return it from the cache.
    
    if(!is.null(inv)) {
        message("Getting the cached inverse.")
        return(inv)
    }
    
    #Otherwise, solve for the inverse and populate the cache.
    
    message("Populating the cache with the inverse.")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    #Return the inverse.
    inv
}


