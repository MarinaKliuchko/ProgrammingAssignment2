## This function creat a list to feed to the next function 
## in order to access if inverse matrix has been cached

makeCacheMatrix <- function(m = matrix()) {
  invert <- NULL #initialize variable to store inverse
  set <- function(y) {   # set stores matrix y in the external environment
    m <<- y
    invert <<- NULL       # and resets the inverse matrix to NULL
  }
  
  get <- function() m #get returns matrix m from the environment
  setinvert <- function(solve) invert <<- solve # stores an argument of inverse matrix
  getinvert <- function() invert #returns an inverse matrixed stored in cache
  list(set = set, get = get, 
       setinvert = setinvert, getinvert = getinvert) #creates a list of the 4 above functions
}

## This function calculates the inverse of a matrix
## created with makeCacheMatrix() or caches in an existent inversion.

cacheSolve <- function(m, ...) {
  invert <- m$getinvert
  
  if(!is.null(invert)) { # gets the cached inverse; skips calculating,
    message("Getting cached data...")   #prints a message
    return(invert)                      #inverse matrix returned
  }
  
  data <- m$get() # calculates the inverse if it is not stored in cash
  invert <- solve(data) #assigns inverted matrix to invert
  m$setinvert(invert) #stors it in cache
  invert # returns the calculated inverse
}

