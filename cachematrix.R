## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. It stores the matrix and its inverse (if computed)
## and provides methods to set, get, and update the matrix and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL # Initialize the inverse as NULL
  
  # Function to set the value of the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y       # Update the matrix
    inv <<- NULL  # Reset the cached inverse to NULL whenever the matrix changes
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getinverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its cached inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" object 
## returned by makeCacheMatrix. It first checks if the inverse has already been 
## computed and cached. If the cached inverse exists, it retrieves the cached value.
## Otherwise, it computes the inverse, caches it, and then returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Retrieve the cached inverse if available
  
  # If cached inverse exists, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse of the matrix
  data <- x$get()  # Retrieve the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setinverse(inv)  # Cache the computed inverse
  inv  # Return the inverse

}
