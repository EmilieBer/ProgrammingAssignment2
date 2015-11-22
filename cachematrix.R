## The following pair of functions calculate the inverse of a matrix and caches (saves) it. 
## This, so that at the next attempt to calculate the inverse of the matrix, 
## the cached inverse is returned instead of running the calculation again. 

## The makeCacheMatrix creates a "special" matrix object that can cache its inverse
## and returns a list containing 4 functions to
## - set the value of the matrix (set)
## - get the value of the matrix (get)
## - set the inverse of the matrix (setinv)
## - get the inverse of the matrix (getinv)
## In this function, values are assigned to objects using <<-.
## This makes sure that the values are assigned in the parent environment, 
## so that the objects can be called in the second function as well.

makeCacheMatrix <- function(x = matrix()) { ## create a function where x is a matrix as default
  m <- NULL ## create the cache m
  set <- function(y) {
    x <<- y ## assigns input matrix y to variable x in the parent environment
    m <<- NULL ## set m to NULL in the parent environment
  }
  get <- function() x ## return input matrix x
  setinv <- function(inv) m <<- inv ## set cache m equal to the inverse of matrix x in the parent environment
  getinv <- function() m ## return the cached inverse matrix m
  list(set = set, get = get, ## put the functions in a list
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function returns the inverse of the "special" matrix that is defined in the makeCacheMatrix function.
## If nothing is stored in cache m yet (m=NULL), the function calculates the inverse of matrix x.
## After that, it sets the cache m equal to the calculated inverse.
## If the inverse is already stored in cache m, the function returns the stored inverse
## together with the message 'getting cached data'.
## In that case, it skips the calculation of the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() ## call the getinv function from the list created in makeCacheMatrix, returning the cached inverse in m
  if(!is.null(m)) { ## If the cache m is not NULL, so an inverse is already cached
    message("getting cached data") ## this message is shown
    return(m) ## and the inverse in cache m is returned
  }
  data <- x$get() ## else, the input matrix x is assigned to 'data'
  m <- solve(data, ...) ## and the inverse of this matrix is calculated
  x$setinv(m) ## here, the inverse is cached in m in the parent environment.
  ## This is necessary, because the inverse in the cache should be found the next time the function is run.
  m ## and the inverse is returned.
}