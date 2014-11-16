## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## return a matrix that is the inverse of 'x'
    m <- x$getinverse()          ## set m to value of inverse
    if(!is.null(m)) {            ## if the inverse (m) isn't null
        message("getting cached data")
        return(m)                ## return it along with a message to user, then end
    }
    data <- x$get()              ## otherwise (if m was null)
    m <- solve(data, ...)        ## solve for the inverse
    x$setinverse(m)              ## set m to the inverse
    m                            ## return m
}

## reminder where file is:  setwd("../ProgrammingAssignment2")
