## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix 
## that transform a normal matrix into a special list containing methods
## for acting as a cache for inverse values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function( y ){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function( inverse ) inv <<- inverse
  getinverse <- function() inv
  list( set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


## Function that takes a special matrix built using *makeCacheMatrix* and calculates 
## its inverse if not solved yet. Otherwise, it calculates the inverse and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if( !is.null( inv ) ){
    message( "getting cached data" )
    return( inv )
  }
  data <- x$get()
  inv <- solve( data )
  x$setinverse( inv )
  inv
}
