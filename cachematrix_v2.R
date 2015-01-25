## This function creates a matrix object that 
## caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## set the value of the matrix
        inv <- NULL
        
        set <- function(y) {
                
                #if(!all((dim(y) == dim(x)))){
                        
                        if(!all(y==x)){
                                
                                inv <<- NULL
                        }
                #}
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


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()
cacheSolve(amatrix)
amatrix$getinverse()
cacheSolve(amatrix)
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) 
cacheSolve(amatrix) 
amatrix$get()  
amatrix$getinverse()


a<-matrix(c(0,5,99,66))
b<-matrix(c(0,5,99,66))
if(!all(a == b)){"1"}else{"0"}

# Test

test <- matrix(runif(9,1,100),3,3)
test2 <- matrix(runif(9,1,100),3,3)

test
test2

testCached <- makeCacheMatrix(test)
testInv <- cacheSolve(testCached)
testInv

test %*% testInv
testCached2 <- makeCacheMatrix(test2)
testInv2 <- cacheSolve(testCached2)
testInv2
test %*% testInv2
testCached$get()
testCached2$get()



crazy <- function() {
        x <<- 3.14                   # variable x in the containing environment (global in this case) is updated to be 3.14
        print(x)                        # since no local variable 'x' exists within function 'crazy' R searches the containing environments
{ print(x);                     # this is to demonstrate the function, not a code block, is the smallest environment in R
  x <- 42; print(x)         # local variable 'x' is declared (created) and assigned the value 42; overrides the variable 'x' in
        }                                  # the containing environment
        print(x)                       # since local variable 'x' now exists within the function there is no need to search the containing
}   

x
x <- 0
crazy()