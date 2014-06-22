## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix 
## that transform a normal matrix into a special list containing methods
## for acting as a cache for inverse values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Define the function that sets a new value for the matrix. New inverse is null since not calculated yet
  set <- function( y ){
    x <<- y
    inv <<- NULL
  }
  # Define function that gets the value of the matrix
  get <- function() x
  # Define function that sets the value of the inverse so that it is cached
  setinverse <- function( inverse ) inv <<- inverse
  # Define function that gets the cached value of the inverse
  getinverse <- function() inv
  # Return a list containing the four defined methods
  list( set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


## Function that takes a special matrix built using *makeCacheMatrix* and calculates 
## its inverse if not solved yet. Otherwise, it calculates the inverse and caches the result

cacheSolve <- function(x, ...) {
  # Get the value of the cached inverse
  inv <- x$getinverse()
  # If value is not null, we have it cached. Return it
  if( !is.null( inv ) ){
    message( "getting cached data" )
    return( inv )
  }
  # Inverse is not cached, get the original matrix and calculate the inverse
  data <- x$get()
  inv <- solve( data )
  # Cache the value of the recently solved inverse operation
  x$setinverse( inv )
  # Return the inverse
  inv
}
