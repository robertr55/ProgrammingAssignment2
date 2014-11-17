## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a (hopefully) n x n invertible matrix x
## and sets up a matrix object to cache the inverse of x


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve will solve or compute the inverse of the matrix returned by
## makeCacheMatrix - if the inverse has already been solved/calculated,
## then the stored inverted matrix is simply returned

cacheSolve <- function(x, ...) { 
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## reminder which directory this file is in:  setwd("../ProgrammingAssignment2")
