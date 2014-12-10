## gets a matrix as an argument,and returns a list of a functions for this matrix
## function set stores matrix in this environment, function get returns the matrix
## set_inverse stores the inverse matrix, and get_inverse returns the inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(inv) {
        inverse <<- inv
    }
    
    get_inverse <- function() inverse
    
    list(set = set, 
         get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)  
}


## Gets a list set by function makeCacheMatrix, and checks if the inverse is 
## calculated before, and if it has been, returns the inverse matrix from cache.
## If the inverse matrix doesn't found it calculate it and store in the cache.

cacheSolve <- function(x, ...) {
    
    inverse <- x$get_inverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
