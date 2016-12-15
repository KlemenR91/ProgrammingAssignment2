## Put comments here that give an overall description of what your
## functions do

## This function creates new matrix with getter and setter functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #INITIALIZE m
    set <- function(y) {
        #FUNCTION SETS ATRIBUTE VALUE TO X AND NULL TO M
        x <<- y
        m <<- NULL
    }
    get <- function() x #RETURN VALUE OF MATRIX
    setinverse <- function(inverse) m <<- inverse  #SETS INVERSE VALUE TO SET INVERSE
    getinverse <- function() m #RETURNS INVERSE VALUE
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## This function returns inverse of matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()   #SET VALUE INVERSE VALUE TO M
    if(!is.null(m)) { #IF INVERSE VALUE ALREADY EXISTS
        message("getting cached data")
        return(m)
    } #IF INVERSE DOESN'T EXISTS
    data <- x$get() #GET MATRIX VALUE
    m <- solve(data, ...) #CALCULATE INVERSE
    x$setinverse(m) #SET INVERSE TO X VARIABLE
    m 
}

#If you want to test this program:
#matrika <- makeCacheMatrix(matrix(c(2,4,3,1,5,7,7,4,2),nrow = 3))
#cacheSolve(matrika)