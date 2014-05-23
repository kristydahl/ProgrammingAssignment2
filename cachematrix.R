# Overall description of what these functions do:

# The makeCacheMatrix and cacheSolve functions calculate the inverse of a matrix (passed as an argument to makeCacheMatrix)
# and store it in a cache (using the set_inverse function within makeCacheMatrix). If the inverse of the matrix is recalculated
# using cacheSolve, the function first checks to see if there is a cached value (using the get_inverse function within 
# makeCacheMatrix) and returns it if there is. If not, it calculates it and sets it using set_inverse.
                                                                        

## makeCacheMatrix is a function that returns a vector with functions as its elements. 

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(y){
      x <<- y
      cached_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) cached_inverse <<- inverse
    get_inverse <- function() cached_inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve takes as an input the vector, x, returned from makeCacheMatrix. cacheSolve then determines whether 
## or not the inverse of the matrix passed in as an argument to makeCacheMatrix has been calculated. If so, it returns
## the cached value. If not, it calculates the inverse and stores it in the cache. 

cacheSolve <- function(x, ...) {
  ## Assign the result of the get_inverse function to the variable cached_inverse. If cached_inverse 
  ## is not NULL (i.e. the inverse for this matrix has been calculated previously), then return the value of 
  ## cached_inverse. If the inverse has not been cached (i.e. cached_inverse is NULL), then the script proceeds to the 
  ## next section.
  
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## If cached_inverse is NULL, the following statements get the original matrix, assign it to the variable "data", 
  ## calculate the inverse of "data" and cache the value of cached_inverse using the set_inverse function. If cacheSolve
  ## is run again for the same matrix, the script would run through the above if function and would return the value
  ## of cached_inverse. 
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}