# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix. 

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## Initialize the inverse property
        ## Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the matrix 
        get <- function() x ## Return the matrix
        setinverse <- function(inverse) inv <<- inverse ## Method to set the inverse of the matrix
        getinverse <- function() inv ## Method to get the inverse of the matrix
        
        ## Return a list of the methods
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## Initialize the inverse property
        if (!is.null(inv)) {
                message("Cached data Getting ....")
                return(inv)
        }
        data <- x$get() # Get the matrix from our object
        inv <- solve(data, ...) ## Calculate the inverse using matrix 
        x$setinverse(inv) ## Set the inverse to the object
        inv # return matrix 
}
