# ref: https://github.com/rdpeng/ProgrammingAssignment2
# File Name: makeCacheMatrix: 
# Function to create a special "matrix" object to cache its inverse.
# sets value of the vector
# gets value of the vector
# sets  value of the mean
# gets value of the mean
makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
  
#  File Name: cacheSolve: 
#  A function to compute inverse of the special "matrix" returned by makeCacheMatrix 
#  Function retrieves inverse from the cache created if
#  inverse is calculated 
#  no change to matrix 
  

cacheSolve <- function(x, ...) 
  {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Check 
m <- matrix(rnorm(12),3,3)
#m <- matrix(rnorm(16),4,4)
m
m1 <- makeCacheMatrix(m)
cacheSolve(m1)