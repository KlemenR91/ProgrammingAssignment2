## Put comments here that give an overall description of what your
## functions do

## This function creates new matrix with getter and setter functions

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


## This function returns inverse of matrix

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

#If you want to test this program:
#matrika <- makeCacheMatrix(matrix(c(2,4,3,1,5,7,7,4,2),nrow = 3))
#cacheSolve(matrika)