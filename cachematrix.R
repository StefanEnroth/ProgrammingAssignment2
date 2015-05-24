## These functions solve a matrix and caches the result. 
## Calling the function to solve the same matrix again will give 
## the cached result.

## This function creates a list of functions for a certain matrix
## For example: testmatrix <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
        # Create a variable that will be used to store the solution
        m <- NULL
        # Create the functions to store/get the matrix and to
        # store/get the solution
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolution <- function(cachesolution) m <<- cachesolution
        getsolution <- function() m
        # Store the 4 functions in a list with named elements
        list(set = set, get = get,
             setsolution = setsolution,
             getsolution = getsolution)

}


## This function takes the object created with the first function as input and 
## computes the solution or retrieves the cached solution if available.
## Continuing with the example from above, cacheSolve(testmatrix) will find the
## solution the first time its run, and subsequent runs will retrieve the
## solution from memory.

cacheSolve <- function(x, ...) {
        m <- x$getsolution()
        # Check if a cached solution is available, and if so return that value
        # and end the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If no cached solution is found, continue and compute the solution
        # and store it for future calls to the function
        data <- x$get()
        m <- solve(data, ...)
        x$setsolution(m)
        m
}
