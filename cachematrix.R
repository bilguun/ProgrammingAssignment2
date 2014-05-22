makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #1. set the value of the matrix  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #2. get the value of the matrix
  get <- function() x
  #3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  #4. get the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the #cachesolve should retrieve the inverse from the cache.
#This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
