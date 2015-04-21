## A pair of functions "makeCacheMatrix" 
## and "cacheSolve" is used to cache the 
## inverse of a matrix inversion to save
## computational time


## This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Function to get the matrix
  get <- function() x
  
  #Function to cache the inverse of the matrix
  setinver <- function(inver) m <<- inver
  
  #Function to retrive the inverse of the matrix
  getinver <- function() m
  
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  m <- x$getinver()
  
  #If inver is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If inver is not cached, solve and cache the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
