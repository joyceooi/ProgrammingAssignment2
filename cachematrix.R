## Below two functions cache the inverse of a matrix.

## function makeCacheMatric create a special "matrix,
## which is a list containing a funtion to
## set/get the value of the matrix
## set/get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        invsx <- NULL
        set <- function(y){
                x <<- y
                invsx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invsx <<- inverse
        getinverse <- function() invsx
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## function cacheSolve compute the inverse of special "matrix",
## returned by function makeCacheMatric
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invsx <- x$getinverse()
        
        ## if the inverse value is not null means has been calculated previously
        ## then return value from cache and skip the calculation
        if(!is.null(invsx)){
                message("getting cache data")
                return(invsx)
        }
        
        ## else calculate the inverse of matrix,
        ## set the inverse value in cache
        ## and finally return the inverse value
        data <- x$get()
        invsx <- solve(data, ...)
        x$setinverse(invsx)
        invsx
}
