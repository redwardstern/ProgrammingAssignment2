
## get or set a matrix A
## get or set the inverse of matrix A
makeCacheMatrix <- function(A = matrix()) {
    inv  <- NULL
    #set matrix A
    set  <- function(y){
            A <<- y
            inv <<- NULL 
    }
    
    #retrieve matrix A
    get  <- function() {
        A
    }
    
    #set the inverse of matrix A
    setinverse  <- function(inverse) {
        inv  <<- inverse
    }
    
    #get the cached inverse of matrix A
    getinverse  <- function() {
        inv
    }
    
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}
            


## create and cache inverse if it doesn't exist
## return inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv  <- x$getinverse()
    
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    
    #inverse has not been set, so create a cache
    data  <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}
            