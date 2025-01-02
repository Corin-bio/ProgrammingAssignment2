
# Function makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Variable to store the cached inverse
    set <- function(y) {
        x <<- y    # Assigns a new matrix to x
        inv <<- NULL  # Resets the cached inverse to NULL
    }
    get <- function() x  # Retrieves the current matrix
    setInverse <- function(inverse) inv <<- inverse  # Stores the inverse in the cache
    getInverse <- function() inv  # Retrieves the cached inverse
    list(set = set,                # Returns a list of functions to interact with the matrix
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# Function cacheSolve: computes the inverse of the special "matrix" created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Checks if the inverse is already cached
    if (!is.null(inv)) {   # If the inverse is cached, retrieve it and skip computation
        message("getting cached data")  # Notify that cached data is being used
        return(inv)  # Return the cached inverse
    }
    mat <- x$get()  # Retrieve the current matrix
    inv <- solve(mat, ...)  # Compute the inverse of the matrix
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse matrix
}

