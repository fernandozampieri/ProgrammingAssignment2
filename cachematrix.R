#Creates makeCacheMatrix function
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
#Creates cacheSolve function using "solve" in order to invert the matrix
#First checks if there is a previous result stored in the cache. Prints a message
#if the returned value is the one previously cached.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    print("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
#Commands to test the functions
#Creates a 3x3 matrix with 9 random integers from 1:100
t1<-matrix(sample(1:100,9),3,3)
var1<-makeCacheMatrix(t1)
cacheSolve(var1)
