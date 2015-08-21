## This file define a set of functions for caching a square matrix and it's inverse


## This function define a set of methods to create and manage 
## a cached matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
      #initialize the inverser value to null
      inv <- NULL
      
      #function to assign the matrix to be cached
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      
      #this function return the cached matrix
      get <- function() x
      
      #assign the calculated inverse matrix to be cached
      setinv <- function(inverse) inv <<- inverse
      
      #retrieve the cached inverse matrix. Will return Null if no value previously cached
      getinv <- function() inv
      
      list(set = set, get = get,
           setinv = setinv, getinv = getinv)

}


## Compute the inverse of a square matrix and cache it for future reuse.
## Cached inverse will be return if already computed previously.
cacheSolve <- function(x, ...) {

      ##Retrieve the cached inverse. Might be null
      inv <- x$getinv()
      
      ##Return the cached inverse if not null
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      
      ##cached inverse is null, let's compute it here...
      ##retrieve the cached matrix
      data <- x$get()
      ##solve the inverse matrix
      inv <- solve(data, ...)
      ##keep cached copy of the inverse matrix
      x$setinv(inv)
      
      ## Return a matrix that is the inverse of 'x'      
      inv  
}
