## makeCacheMatrix creates an enhanced matrix object that caches the inverse of the matrix
## The cached inverse is updated when the cacheSolve() function is first called.
## Subsequent calls to the cacheSolve() function will return the cached inverse as long as the matrix is unchanged.
## The matrix can be changed with the set and get methods.

## makeCacheMatrix() returns a special matrix object which can cache the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
inverseMatrix <- NULL ##reset the cached inverseMatrix
set <- function(y) {
  x <<- y ##sets this matrix to the supplied matrix
  inverseMatrix <<- NULL ##reset the cached inverseMatrix as the matrix has just changed
}
get <- function() x ##return the current matrix
setInverse <- function(solve) inverseMatrix <<- solve ##solve for the Inverse and assign to the cached InverseMatrix
getInverse <- function() inverseMatrix ##return the cached inverseMatrix
list(set = set, get = get, ##return all the functions for getting and setting the matrix and its inverse as the return value for the makeCacheMatrix function
     setInverse = setInverse,
     getInverse = getInverse)
}


## cacheSolve() returns the cached inverse of the matrix if one exists, else it solves for the inverse and returns it while also updating the solved inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse() ##get the cached inverseMatrix
  if(!is.null(inverseMatrix)) { ##check if the cached inverseMatrix was null
    message("getting cached data") ##OK we got something non null
    return(inverseMatrix) ##return it.. all done
  }
  data <- x$get() ##since we got a null cached inverseMatrix, lets get the matrix into data
  inverseMatrix <- solve(data, ...) ##lets calculate the inverse using solve 
  x$setInverse(inverseMatrix) ##set the obtained inverse into the cache
  inverseMatrix ##return the newly calculated cached value
}
