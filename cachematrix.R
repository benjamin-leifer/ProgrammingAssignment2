## Put comments here that give an overall description of what your
## functions do
## As assigned, these functions make objects that 

## Write a short comment describing this function
## This function creates an object that contains getter and setter functions
## for a matrix and it's inverse. The inverse of the matrix is also cached when
## it is created, so that it can be pulled from the cache in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## This function takes the matrix object created in makeCacheMatrix
## checks if the inverse has been cached, returns it if it is, 
## and if not, calculates, caches, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

