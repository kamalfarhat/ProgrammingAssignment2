##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix returns a list of 3 functions:
##1- Get the value of the matrix
##2- Get the value of the inverse
##3- Set the value of the inverse (store in Cache)

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve checks if the inverse has already been calculated, if yes, retrieve it from cache, if no, calculate it and store it in cache .

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inverse <- x$getInverse()

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
  
}
