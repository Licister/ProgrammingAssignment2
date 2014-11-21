# These functions take a matrix and calculate its inverse. The inverse is then stored in cache so it
# can be easily retrieved without the need to calculate it again. 

# makeCacheMatrix stores a matrix and its inverse in a single object. The inverse is only stored after
# cacheSolve is called, since the inverse is only calculated in cacheSolve. makeCacheMatrix allows 
# cacheSolve to store the calculated inverse to avoid spending time calculating it again in the future.


makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() {x}
      
      setsolve <- function(solve) {
            inv <<- solve
      }
      
      getsolve <- function() {inv}                      
      
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

# cacheSolve returns the inverse of the matrix.
# First, it gets the inverse of the matrix stored in cache. It will be NULL if it hasn't been calculated yet, or
# an actual inverted matrix if it has. 
# If the inverse isn't NULL, the function will return the inverted matrix and end the function. 
# If it is NULL, it'll skip the 'if' statement, get the original matrix from 'x', and calculate the inverse. 
# After that, it'll store it in cache so it can be accessed later, and finally it'll return the inverse.

cacheSolve <- function(x, ...) {
      
      inv <- x$getsolve()
      
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      data <- x$get()
      
      inv <- solve(data, ...)
      
      x$setsolve(inv)
      
      inv
}