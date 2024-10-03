# Function to store a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  
  set <- function(y) {  # Set a new matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x  # Get the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Get the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to calculate or get the cached inverse
cacheSolve <- function(x) {
  inv <- x$getInverse()  # Try to get the cached inverse
  
  if(!is.null(inv)) {    # If cached, return it
    message("cached")
    return(inv)
  }
  
  data <- x$get()        # Get the matrix
  inv <- solve(data)     # Calculate the inverse
  x$setInverse(inv)      # Cache the inverse
  inv                    # Return the inverse
}
