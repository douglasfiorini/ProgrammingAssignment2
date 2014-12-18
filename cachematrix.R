## Make cache of the matrix

## this function has the purpose to cache the result of the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {  
  xInversed <- NULL  
  
  get <- function() x
  
  set <- function(x1) {
    x <<- x1;
    xInversed <<- NULL
  }
  
  getInverse <- function() xInversed  
  
  setInverse <- function(m) xInversed <<- m
  
  list(get=get,getInverse=getInverse,setInverse=setInverse)
}


## It'll try to solve the inverse of the given matrix and return the result. 
## It'll keep a cache of the result in case of a second called

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  calculatedInv <- x$getInverse()
  if (!is.null(calculatedInv)){
    message("getting cached data")
    calculatedInv
  }else{
    calculatedInv <- solve(x$get(), ...);
    x$setInverse(calculatedInv)
    calculatedInv
  }
}
