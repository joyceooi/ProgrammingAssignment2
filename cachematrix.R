## Objective of below two functions are to cache the inverse of a matrix.

## function makeCacheMatric create a special "matrix,
## which is a list containing a funtion to
## set/get the value of the matrix
## set/get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize inverse matrix
        invsx <- NULL
        
        ## set the value of the matrix
        set <- function(y){
                x <<- y
                invsx <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse matrix
        setinverse <- function(inverse) invsx <<- inverse
        
        ## get the value of the inverse matrix
        getinverse <- function() invsx
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## function cacheSolve compute the inverse of special "matrix"
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
