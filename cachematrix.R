## cachematrix.R
# This file contains two functions; 
# makeCacheMatrix - Creates a matrix that can also stores it's inverse
# cacheSolve - Solve for the inverse of a matrix but return it from cache if
#             one exists.

## makeCacheMatrix(x)
# This is inspiried by the example code. We use a list to store a set of accessors
# and a set of accessores for the inverse of the matrix. The matrix and inverse are
# stored in the parent environment avoiding local assignment issues.
makeCacheMatrix <- function(x = matrix()) {
	## we are going to create a new object that is have a matrix and a list
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvs <- function(invs) m <<- invs
  getinvs <- function() m
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## cacheSolve(x)
# This is also inspired by the example code. We test if we have a inverse for the input matrix. If it exists
# we return it. Else we use the accessors from makeCachematrix to populate the inverse.
cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvs(m)
  m
}
