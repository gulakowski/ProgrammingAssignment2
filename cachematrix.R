## Put comments here that give an overall description of what your
## functions do

## inputs : matrix
## Output : Creates list that contains fuctions to :
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse matrix
    ## get the value of the inverse matrix.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(x) m <<- x
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
} ## end makeCacheMatrix


## Checks matrix to see if inverse has been solved, if solved - returns inverse, else 
## computes and returns inverse.  

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    q<-if(!is.null(m)) {
        message("getting cached data")
        return(m)
        } ## end if
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
        ## Return a matrix that is the inverse of 'x'
} ## end cacheSolve
