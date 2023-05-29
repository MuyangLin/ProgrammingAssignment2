## Caching the Inverse of a Matrix

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set_matrix <- function(new_value) {
                x <<- new_value
                cache <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(solve) cache <<- solve
        get_inverse <- function() cache
        
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get_matrix()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}