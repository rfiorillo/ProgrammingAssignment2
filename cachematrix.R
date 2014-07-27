## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #initialize m
  m <- NULL
  
  #define set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #define get function
  get <- function() x
  
  #define setsolve function
  setsolve <- function(solve) m <<- solve

  #define getsolve function
  getsolve <- function() m
  
  #return functions in a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cacheSolve: This function computes the inverse of the special
#"matrix" returned by makeCacheMatrix above. If the inverse has
#already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache
#for performance.
cacheSolve <- function(x, ...) {
  #get cached inverse if exists
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #otherwise use solve to calculate the inverse and cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
