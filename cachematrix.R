## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the matrix
#get the matrix
#set the inverse of the matrix
#get the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


#The following function calculates the inverse of a matrix 
#created with the above function. However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse from 
#the cache and skips the computation. Otherwise, it calculates the 
#inverse and sets the value of the inverse in the cache via the 
#setinv function.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
        ## Return a matrix that is the inverse of 'x'
}
