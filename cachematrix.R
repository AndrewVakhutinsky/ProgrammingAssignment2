## function makeCacheMatrix
# creates a special list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m1 <<- inv
  getInv <- function() m1
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## function cacheSolve
#  calculates the inverse of the matrix contained in the list created with the function makeCacheMatrix. 
#  However, it first checks to see if the inverse has already been calculated. 
#  If so, it gets the inverse from the cache and skips the computation. 
#  Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
#  via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m1 <- x$getInv()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data)
  x$setInv(m1)
  m1
}
