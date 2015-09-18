## Functions to calculate inverse of a matrix and store result into cache for later reuse
## makeCacheMatrix: creates an environment and list of functions to store and get cache data
## cacheSolve: if available, returns the cached inverse of the matrix,
##             else calculates and stores it in cache for later reuse
## usageEx: runs cacheSolve twice and show the performance gain of cached version

makeCacheMatrix <- function(x = matrix()) {
  mt_inv <- NULL # initializes once the matrix inverse variable
  set <- function(y) {
    x <<- y # sets a matrix of which the inverse must be calculated, and stores in parent environment
    mt_inv <<- NULL # re-sets mean variable in parent environment
  }
  get <- function() x # returns the value of x set by the call to makeCacheMatrix(x)
  setsolve <- function(inverse) mt_inv <<- inverse # stores inverse in parent environment, where mt_inv exists
  getsolve <- function() mt_inv # returns the latest calculated inverse (if any)
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # creates list of functions
}

cacheSolve  <- function (x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mt_inv <- x$getsolve() # retrieves mt_inv from cache
  if(!is.null(mt_inv)) { # checks if the retrieved object is NULL
    message("getting cached data")
    return(mt_inv) # return cached version of the inverted matrix
  }
  message("calculates inverse - no cache available")
  mt <- x$get() # gets data from the function closure
  mt_inv <- solve(mt, ...) # calculates the inverse of the data matrix
  x$setsolve(mt_inv) # stores the calculated inverted matrix in the cache
  mt_inv # return the just calculated version of the inverted matrix
}

### Example of execution ----
usageEx <- function (dimension = 1000) {
  mt_dim <- dimension
  matrix <- makeCacheMatrix(matrix(rnorm(mt_dim^2), mt_dim, mt_dim))
  for (i in 1:2) print(system.time(cacheSolve(matrix))) # repeat call to cacheSolve twice, cached version is must faster
}
