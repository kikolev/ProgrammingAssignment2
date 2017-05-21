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
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m
}
