## makeCacheMatrix is a function that is passed a square invertible matrix, and
## returns a list of 4 functions: set, get, setInverse, getInverse

## Function definition - default argument value is an empty matrix

makeCacheMatrix <- function(A = matrix()) {
  
  
  ## Initialise the Inverse to NULL
  I <- NULL
  
  ## set function: Sets the matrix A passed to the function to a new value x
  ## and sets the inverse to NULL ( the old value is no longer valid)
  set <- function(x) {
    A <<- x
    I <<- NULL
  }
  
  ## get function: Gets the value of the matrix A passed to the function
  get <- function() A
  
  ## setInverse function: sets the inverse to a new value - inverse
  ## NOTE: This may not be the actual inverse of the matrix passed to
  ##       the function. The inverse will be set to whatever is passed
  ##      as an argument to this funtion.
  setInverse <- function(inverse) I <<- inverse
  
  ## getInverse function: gets the current value of the inverse.
  getInverse <- function() I
  
  ## Put each of the new functions into a list
  ## This list is returned by the makeCacheMatrix function
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that is passed a list object, which has been created
## by the makeCacheMarix funtion)
## The function either returns the cached matrix inverse, or, if there is nothing
## in the cache, it calculates the new matrix inverse and writes it to the cache

cacheSolve <- function(x, ...) {
  ## get the value of the inverse from the cache
  I <- x$getInverse()
  ## If there is a value stored in the cache, return it (dont compute again)
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  ## If there isn't anything stored in the cache, then calculate the inverse again,
  ## and store in the cache (I)
  ## NOTE: As per the assignment brief, it is assumed that the matrix is always 
  ##      invertible. One way to check for a non-invertible i.e. singular matrix
  ##      is to check that the matrix determinant is non-zero. 
  
  ## get the matrix
  data <- x$get()
  ## calculate the inverse
  I <- solve(data, ...)
  ## save new value to cache
  x$setInverse(I)
  
}