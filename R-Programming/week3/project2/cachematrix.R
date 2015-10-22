
## Written by: Michael Gregory
## Email: milesper@gmail.com
## For coursera data science R programming class, week 3, programming assignment #2

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) matinv <<- inverse
        getinv <- function() matinv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## NOTE: We assume that the matrix supplied is always invertible !!!!!!
        
                matinv <- x$getinv()
                if(!is.null(matinv)) {
                        message("getting cached data")
                        return(matinv)
                }
                data <- x$get()
                
                ## Computing the inverse of a square matrix can be done with the solve function in R. 
                ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
                
                matinv <- solve(data, ...)
                        
                x$setinv(matinv)
                matinv
                
}
