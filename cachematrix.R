# Week 3 assignment.

# Equivalent functionality for matrices.

# testFunction() function (optional two matrix input for testing resetting/caching) 
#   steps through the two functions

## makeCacheMatrix
# Input: matrix
# Output: a `matrix` with the four additional methods listed below
# Methods:
#   $set() = 
#   $get() = 
#   setinverse() = sets the inverted matrix to the cache
#   getinverse() = gets the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheInverse
# Input: a matrix
# Output: Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    print(x)
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## testFunction
# Input: Optional list of matrices
# Steps through the two functions 

testFunction <- function(input_matrix = matrix(c(1, 2, 3, 4), nrow = 2), 
                         replacement_matrix = matrix(c(5, 6, 7, 8), nrow = 2)) {
    
    cat("\nOriginal matrix\n\n")
    print(input_matrix)
    
    my_matrix = makeCacheMatrix(input_matrix)
    cat("\n\n$get retrieve value\n\n")
    print(my_matrix$get())
    
    cat("\n\n$getinverse, which should initially be NULL\n\n")
    print(my_matrix$getinverse())
    
    cat("\n\n$set, to reset with replacement matrix\n")
    my_matrix$set(replacement_matrix)
    
    cat("$get, with updated replacement matrix\n\n")
    print(my_matrix$get())
    
    cat("\n\ncacheInverse, for solve / inverted matrix\n\n")
    cacheSolve(my_matrix)
    
    cat("\n\n$getinverse, now not NULL following cacheSolve() call\n\n")
    my_inverse = my_matrix$getinverse()
    print(my_inverse)
    
    cat("\n\nnow recapitulating replacement_matrix using prior cacheSolve() matrix output\n\n")
    my_matrix$set(my_inverse)
    print(my_matrix$get())
    cacheSolve(my_matrix)
}
