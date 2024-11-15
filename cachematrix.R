# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)  # Cache the inverse
  
  # Return the inverse
  inv
}

#Test Cases

# Define an invertible matrix
mat <- matrix(c(1, 2, 3, 4), 2, 2)
cacheMat <- makeCacheMatrix(mat)


# First call should compute the inverse
inverse1 <- cacheSolve(cacheMat)
print(inverse1)

# Second call should use cached data
inverse2 <- cacheSolve(cacheMat)
print(inverse2)






