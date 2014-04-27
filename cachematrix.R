# These two functions are used to compute the inverse of a matrix and store
# the result in a cache for later reuse. 

#*******************************************************************************
#
# Function makeCacheMatrix
#
# Description:
# This function creates a matrix 'wrapper' object used to store a matrix and
# its inverse.
#
# Arguments:
# mat       A square numeric or complex matrix.
#
# Returns:
# A list of 4 functions to set and retrieve the matrix itself and its inverse.
#
# Example usage:
# > m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# > m$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m$set(matrix(c(1,1,1,2), nrow=2, ncol=2))
# > m$get()
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    2
# > m$setinverse(matrix(c(2,-1,-1,1), nrow=2, ncol=2))
# > m$getinverse()
#      [,1] [,2]
# [1,]    2   -1
# [2,]   -1    1

#*******************************************************************************

makeCacheMatrix <- function(mat = matrix()) {
        
        # Variable that stores the inverse matrix
        inv <- NULL
        
        # Function to set the matrix
        set <- function(m) {
                mat <<- m
                # Set the inverse matrix to NULL, effectively removing it from
                # the cache.
                inv <<- NULL
        }
        
        # Function to get the matrix
        get <- function() {
                mat
        }
        
        # Function to set the inverse matrix
        setinverse <- function(i) {
                inv <<- i
        }
        
        # Function to get the inverse matrix
        getinverse <- function() {
                inv
        }
        
        # Return a list containing the 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#*******************************************************************************
#
# Function cacheSolve
#
# Description:
# This function computes the inverse of a matrix, using a cached value
# if available. It uses the solve function from the base package to perform
# the actual computation.
#
# Arguments:
# x         A matrix 'wrapper' object created with the makeCacheMatrix function.
# ...       Other arguments that are passed down to the solve function.
#
# Returns:
# The inverse of the matrix 
#
# Example usage:
# > m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m) %*% m$get()
# getting cached data
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
#
#*******************************************************************************

cacheSolve <- function(x, ...) {
        
        # Get the inverse matrix from the cache
        inverse <- x$getinverse()
        
        # Check if the cache contained a value
        if(!is.null(inverse)) {
                message("getting cached data")
                
                # Return the cached inverse
                return(inverse)
        }
        
        # Get the matrix
        data <- x$get()
        
        # Compute the matrix inverse
        inverse <- solve(data, ...)
        
        # Store the inverse in the cache
        x$setinverse(inverse)
        
        # Return the inverse
        inverse
}
