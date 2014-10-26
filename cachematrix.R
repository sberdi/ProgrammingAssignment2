## The following functions caches and computes the inverse of a matrix

## This function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    mi <- NULL

    #set the matrix
    set <- function(matrix) {
     x <<- matrix
     mi <<- NULL
    }

    #get the matrix
    get <- function() { 
    x 
    }

    #set the inverse of the matrix
    setinverse <- function(inverse) {
    mi <<- inverse
    }

    #get the inverse of the matrix
    getinverse <- function() {
    mi
    }

    ##return list of the matrix and inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes inverse of the matrix returned by the makeCacheMatrix function. It will return the inverse of the cached object if it has already been calculated
cacheSolve <- function(x, ...) {
    # assign the inverse of x
    m <- x$getinverse()

    # if inverse is set, get cached data
    if(!is.null(m) ) {
    message("getting cached data")
    return(m)
    }

    # call the get method and assign to data
    data <- x$get()

    # compute the inverse of the square matrix with the solve function
    m <- solve(data) %*% data

    # set the inverse of x
    x$setinverse(m)

    #return the matrix set
    m
}