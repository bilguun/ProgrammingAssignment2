# Matrix inversion is costly, therefore caching the inverse of matrix has its benefits. 
# Follwing two functions are used to cache the inverse of a matrix 


# Function to cache inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  # initialize an empty cache upon creation of special matrix  
  inv <- NULL
  
  set <- function(y) {
   
   # exit if argument is not a matrix
   if(!is.matrix(y)){
            return(message("...not a valid matrix, try again..."))}
    
    # overwrite existing matrix with new matrix
    x <<- y
  
    # set cache to NULL if a new matrix is created using set()
    inv <<- NULL
  }
  
  # retrieve existing matrix x 
  get <- function() x
  
  # insert inverse into cache
  setinverse <- function(inverse) inv <<- inverse
  
  # retrieve existing cache
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the #cachesolve should retrieve the inverse from the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## retrieve the value of the cache in x and return that value
  ## if it exists (not Null)
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## if cached value does not exist (is null), then get the value
  ## of the matrix from specialMatrix and use the solve function
  ## to return the inverse i and store it in cache
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
