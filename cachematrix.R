## Lexical Scoping Assignment2: Caching the Inverse of a Matrix
## created by: 
## Dec 11, 2017

## Function 1: This creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse_mtrx <- NULL
        set <- function(y){
                ## from the mean example, (1)set the value of matrix
                x <<- y
                inverse_mtrx <<- NULL
        }
        ## (2) get the value/s of the matrix
        get <- function() x 
        ## (3) set the value/s of the inverse of matrix
        setinverse <- function(inverse) inverse_mtrx<<- inverse
        ## (4) get the value/s of the inverse of matrix
        getinverse <- function() inverse_mtrx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function 2: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. cachesolve retrieves the inverse from  
## the cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## (1) check to see if the inverse has been calculated in 1st func
        ## if so, it gets the inverse from the cache and skips computation
        
        inverse_mtrx <- x$getinverse()
        if (!is.null(inverse_mtrx)){
                message("getting cached data")
                return(inverse_mtrx)
        }
        
        data <- x$get()
        ## else, it calculates the inverse of the matrix(x) and 
        ## sets the value of the inverse in the cache via the setinverse function
        
        inverse_mtrx <- solve(data, ...)
        x$setinverse(inverse_mtrx)
        
        return(inverse_mtrx)
}
