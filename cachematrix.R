## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## two functions makeCacheMatrix and cachedSolve used to create a matrix object makeCacheMatrix
## makeCacheMatrix is S3 object with getter and setter functions for matrix and mmatrix inverse
## call to cacheSolve will check is the matrix object has been used to solve for matrix inverse
## and if the inverse of matrix exist in cache memory.
## if inverse of matrix already exist in memory than the getinv() retreves object form memory



## makeCacheMatrix creates a special S3 object with getter and setter functions
##get() and set() functions initiallizes and retreves the matrix object
## getminv() and setminv() methods used to retreve and set matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(minv) m <<- minv
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## THis function takes in makeCacheMatix S3 oject and check to see if the object
## already exist into memory cahce and if yes then it retrives from memory otherwise
## the matrix inverse is computed using the solve() function with takes matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Data was not found in the cache above...solve it
        
        data <- x$get()
        
        # Check if matrix can be inversed. Singular matrixes 
        # cannot be inversed. No point in p roceeding otherwise. 
        
        f <- function(j) class(try(solve(j),silent=T))=="matrix"
        
        if(f(data)) {      
        
                m <- solve(data, ...)
                x$setminv(m)
                m
        }
        else
        {
                # Provide a meaningful error and bail out
                message("Matrix is singular and cannot be inversed")
                stop("Retry with different data")
        }
}

