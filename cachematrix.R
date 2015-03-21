## This file contains two functions. The first function creates a special 
## "matrix" object which contains the actual matrix and caches its inverse.
## This "matrix" object also has functions for getting and setting the matrix
## itself and its inverse.
## The second function accepts the special "matrix" object as an input argument,
## calcuates its inverse (if the object has not had its inverse cached) and
## stores the inverse back into the object.

## makeCacheMatrix creates a special "matrix" object which contains the
## actual matrix and its inverse.
## Input of the function: x as a matrix
## Output of the function: a list of functions whose names are as follows:
##    get() gets the actual matrix
##    set(y) sets the matrix of this object to y and resets the internal
##    inverse cache
##    getInverse() retrieves the matrix inverse cached
##    setInverse(inverseMatrix) sets the internal cache to inverseMatrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function()
    {
        x
    }
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL    
    }    
    
    getInverse <- function()
    {
        inv
    }    
    
    setInverse <- function(inverseMatrix)
    {
        inv <<- inverseMatrix
    }
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve accepts a special "matrix" object and calculates its inverse (if
## the object does not have the inverse cached). It then stores the calculated
## inverse back to the object.
## Input:   x is the special "matrix" object
##          ... are the extra arguments passed to the solve() function for
##          finding the matrix inverse
## Output:  Returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Check if the x object already contains a cached inverse
    inverse <- x$getInverse()
    
    if (!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)        
    }
    
    ## Here the inverse matrix is null. Need to calculate the inverse    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    
    ## Set the inverse back to the object
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
