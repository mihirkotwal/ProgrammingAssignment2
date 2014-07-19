## Provides cached inverse matrix objects to reduce duplicate computation.
## Assumption: Input matrix is always invertible.
## Code based on example in README.md file.

## Creates extended matrix object which can store the inverse matrix
## in addition to the original input matrix.
makeCacheMatrix <- function(x = matrix()) {
        # Create variable for cached inverse matrix
        inv <- NULL
        
        # Define setter and getter for input matrix
        set <- function(y) {
                x <<- y
                
                # Clear cached inverse matrix when input matrix changes
                inv <<- NULL
        }
        get <- function() x
        
        # Define setter and getter for inverse matrix
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        # Return object function list
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Return the inverse of an input matrix. If result is cached and input
## matrix has not changed, return cached result instead of recalculating
## result.
## If result is not cached, calculate the inverse matrix and store result
## in memory for future use.
cacheSolve <- function(x, ...) {
        # Read variable for cached inverse matrix
        inv <- x$getinv()
        
        # Check if inverse matrix is cached
        if(!is.null(inv)) {
                # If inverse matrix is cached, then return cached result
                #message("getting cached data")
                return(inv)
        }
        
        # Inverse matrix is not cached
        # Calculate inverse matrix from input matrix
        input <- x$get()
        inv <- solve(input, ...)
        
        # Save result to variable for cached inverse matrix
        x$setinv(inv)
        
        # Return a matrix that is the inverse of 'x'
        inv
}
