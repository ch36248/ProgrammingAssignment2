## cachematrix.R
## R functions which provide functionality to set and calculate
## the inverse of a matrix

## makeCacheMatrix
## Main function which returns 4 different functions to initialize the matrix
## get the value of the matrix, set the value of the inverse of the matrix, and 
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
## Function which returns a cached copy of a previously calculated 
## matrix inverse (if available).  If  not available, the function
## calculates the inverse and stores it for future use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
