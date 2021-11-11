## Put comments here that give an overall description of what your
## functions do

## chaching the matrix inverse instade of calculating it
## everytime it is needed.

makeCacheMatrix <- function(mat = matrix()) {
  
  inverse <- NULL
  
  get <- function() mat
  
  set <- function(newmatrix) {
    mat <<- newmatrix
    inverse <<- NULL
  }
  
  get.inverse <- function() inverse
  
  set.inverse <- function(newinverse) inverse <<- newinverse

  list(get = get, set = set,
       get.inverse = get.inverse,
       set.inverse = set.inverse
       )
}


## Rutrn the cahced `inverse` value, if it is cached\
## otherwise it calculates the inverse , cache it
## and return it.

cacheSolve <- function(cached.mat, ...) {

  inverse <- cached.mat$get.inverse()

  #retrived from cache.
  if(!is.null(inverse)) {
    return(inverse)
  }
  
  #calculated and cached.
  raw.mat <- cached.mat$get()
  inverse <- solve(raw.mat, ...)
  cached.mat$set.inverse(inverse)
  
  inverse
}
