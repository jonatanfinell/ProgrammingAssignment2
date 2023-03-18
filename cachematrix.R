# Programming Assignment 2

## Write a short comment describing this function

# makeCacheMatrix() can create a new matrix and returns a list of functions that
# that can be used to modify and retrieve values from the matrix. It can also
# compute and retrieve its (=matrix) inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix value
  set <- function(y) {
    x <<- y  # introduce the <<- operator - can assign a value to an object in 
             # the global environment (not only local)
    inv <<- NULL
  }
  
  # Function to get the matrix value
  get <- function() x
  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Return a list of functions
  list(set = set,
       get = get,
       getInverse = getInverse,
       setInverse = setInverse)

}





## Write a short comment describing this function

# cacheSolve()- function can compute the inverse of the matrix and cache
# the result. It first checks if the inverse has already been calculated
# and cached. If inverse has been cached, the cached value will be returned.
# Otherwise, the function calculates the inverse using the solve()-function 
# and caches the result using the "setInverse" function.

# cachesolve() 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # ... if it exists
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If cached inverse doesn't exist, compute it using the "solve()"-function 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the inverse
  inv
}


###############################################################################
#### putting functions to work

# Create a matrix and cache its inverse
m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

cacheSolve(m) # Output same as calling: solve(matrix(c(1, 2, 3, 4), nrow=2))
              
# Confirm results
solve(matrix(c(1, 2, 3, 4), nrow=2))

# Change the matrix and compute its inverse again
m$set(matrix(c(3, 4, 5, 6), nrow = 2))
cacheSolve(m)

