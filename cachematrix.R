## Put comments here that give an overall description of what your
## functions do

## These functions will Cache the Inverse of a Matrix

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}

## Write a short comment describing this function
##This function computes the inverse of the special"matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then `cacheSolve` will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
