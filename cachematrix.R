## Put comments here that give an overall description of what your
## functions do
## These functions written in partial fulfillment of Coursera Data Science: R Programming 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse
    
    makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                              
        set <- function(y) {                   
            x <<- y                            
            inv <<- NULL                       
        }
        get <- function() x                     
        
        setinverse <- function(inverse) inv <<- 
            getinverse <- function() inv                     
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
    }
    
    
    ## Write a short comment describing this function
    ## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix 
    ## If the inverse has already been calculated (and the matrix has not changed),
    ## then cacheSolve will retrieve the inverse from the cache and not compute the inverse of the matrix
    
    cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
}