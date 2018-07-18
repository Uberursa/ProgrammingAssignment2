## Put comments here that give an overall description of what your
## functions do

## This builds an 'object' that has functional methods for setting and getting both
## the matrix (x) and the solved inverse (inv) in order to save the time for computing 
## inverse on big matrices 
## Do not call the getInverse or setInverse function outside cacheSolve. The stored values
## have no error checking in place for van  dalizing the stored inverse, and if not 
## computed and stored before will only return NULL
## When givin the object a new matrix (x) using the set function, the stored matrix will
## be deleted since it is not garunteed to be correct.
## The get function returns the matrix (x)
makeCacheMatrix <- function(x = matrix()) {
  ## catches the later 'inv <<-' so it does not go to global
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  ## sets the inv variable in the 
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function () inv
  list(set = set
       , get = get
       , setInverse = setInverse
       , getInverse = getInverse
       )
}


## COWARDS WHO WORSHIP AT THE ALTAR OF C
## COWER BEFORE MY FUNCTIONAL POWER
## WHILE YOU WERE WRITING FACTORIES
## I WAS FLYING SPACESHIPS
cacheSolve <- function(x, ...) {
  if (!is.null(x$getInverse())) {
    message("retrieving cache")
  } else {
    x$setInverse(solve(x$get()))
  }
  x$getInverse()
}
