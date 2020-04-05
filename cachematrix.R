## Put comments here that give an overall description of what your
## functions do

## This function contains setter and getter methods for the values
## of a matrix.  It also allows for the inverse of the matrix to be
## set and retrieved (get).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # set inverse to NULL

  # this is the setter function.
  set <- function(y){
    x <<- y #Given a matrix y, assign it to x (the input to the parent function)
    inv <<- NULL # When provided a new matrix, set the inverse to NULL
  }
  get <- function() x # retrieves the matrix
  set_inv <- function(x_inv = matrix()){ 
    inv <<- x_inv # assigns the given inverse in this function to
                  # the 'inv' in the parent function
  }
  get_inv <- function() inv # retrieves the inverse matrix
  
  # This names and returns the elements of the "matrix" which is 
  # actually a list of functions and cached values
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv, 
       inv = inv)
}


## Given an 'matrix' in the special form created above, this
## function will return the inverse, after first checking whether
## the inverse has been previously calculated.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()  # return the inverse matrix
  if(!is.null(inv)) { # check to see if the inverse is already stored
      message("getting cached inverse...") 
      return(inv) # return previous solution (do not recompute)
  }
  # only if the inverse has not already been computed does this execute:
  data <- x$get() # get the matrix
  inv <- solve(data) # compute the inverse
  x$set_inv(inv) # set the inverse as part of the special matrix above
  return(x$get_inv()) #return the solution (inverse matrix)
}
