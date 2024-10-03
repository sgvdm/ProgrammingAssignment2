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
  
  #these are the keys which will be used in cacheSolve function below
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 
}

# Function to calculate or get the cached inverse. This function computes 
#the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#If the function detects that the matrix is already cached, it prints message.
cacheSolve <- function(x) {
  inv <- x$getInverse()  # Try to get the cached inverse
  
  if(!is.null(inv)) {    # If cached, return it
    message("cached")
    return(inv)
  }
  
  data <- x$get()        # Get the matrix using function in makeCacheMatrix
  inv <- solve(data)     # Calculate the inverse.[solve(X) returns its inverse]
  x$setInverse(inv)      # Cache the inverse using function in makeCacheMatrix
  inv                    # Return the inverse. [Last item in function returned]
                         # and it will be inverted due to solve.  
  }

#### Testing of functions
TestMatrix <- matrix(c(2, 3, 3, 4), nrow = 2, ncol = 2)
cachedMatrix <-(makeCacheMatrix(TestMatrix)) #Create matrix & caches its inverse
cacheSolve(cachedMatrix) #Will return square matrix after testing for cache
cacheSolve(cachedMatrix) #Since this is second time, will print cached.

###Returns
#[,1] [,2]
#[1,]   -4    3
#[2,]    3   -2
#> cacheSolve(cachedMatrix) #Since this is second time, will print cached.
#cached
#[,1] [,2]
#[1,]   -4    3
[#[2,]    3   -2