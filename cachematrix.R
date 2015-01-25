## This function creates a matrix object that 
## caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## set the value of the matrix
        inv <- NULL
        
        set <- function(y) {
                
                if(!all(y==x)){
                        
                        inv <<- NULL
                }
                
                x <<- y
        }
        
        ## get the value of the matrix, no arguments
        get <- function() { 
                x }
        
        ## set the inverse of the matrix
        setinverse <- function(solve) { 
                inv <<- solve }
        
        ## get the inverse of the matrix
        getinverse <- function() { 
                inv }
        
        #create a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of object returned
## by the function makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix has not 
## changed), then the function cachesolve retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## try to get the inverse         
        inv <- x$getinverse()
        
        ## check if not na 
        if(!is.null(inv)) {
                
                message("cached data")
                return(inv)
                
        } 
        
        ## else get the inverse  
        data <- x$get()
        inv <- solve(data, ...)
        ## set the inverse  
        x$setinverse(inv)
        return(inv)        
}

