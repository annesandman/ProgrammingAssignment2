## Create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Clear the value of the inverse
  inv <- NULL
  
  # Reset conditions for running $set later
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
  # Define the "get" function for retrieving the matrix later
  get <- function() x
  
  # Define functions for setting and getting the inverse
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  
  # Now make all these functions available in list form
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Retrieve or, if necessary, compute the inverse of the supplied matrix 'x'

cacheSolve <- function(x, ...) {
  # First retrieve whatever's stored as the inverse in variable x
  inv <- x$getinv()
  
  # Check to see if it exists or is null
  if(!is.null(inv)) {
    # If it exists, print a message to console and return the value
    message("Retrieving cached data")
    return(inv)
  }
  
  # Otherwise (if inv is null), compute the inverse now
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse for later use
  x$setinv(inv)
  
  # Finally, return the value
  inv
  
}
