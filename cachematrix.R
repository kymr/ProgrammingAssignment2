## Matrix inversion is usually a costly computation.
## There are 2 functions that calculate the inverse of matrix efficiently with cache.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
    inverse_matrix <- NULL
    set <- function(new_m) {
        m <<- new_m
        inverse_matrix <<- NULL
    }
    
    get <- function() m
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
    inverse_matrix <- m$getinverse()
    
    if (!is.null(inverse_matrix)) {
        return(inverse_matrix)
    }
    
    data <- m$get()
    inverse_matrix <- solve(data, ...)
    m$setinverse(inverse_matrix)
    inverse_matrix
}

