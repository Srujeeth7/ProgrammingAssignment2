## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## set the cache value to null
	cache <- NULL
  set <- function(y){
  ## set the values of matrix 
      x <<- y
	## the cache value is null for the first time  
      cache <<- NULL
  }
  get <- function() x ## return the matrix
  setInverse <- function(mat){
  ## sets the cache value for the first time if not set before
    cache <<- mat
  }
  getInverse <- function() cache ## Return the cache value which is set before
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  ## Return the list 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## sets the inverse of the matrix if cached
	inverse <- x$getInverse()
	  if(!is.null(inverse)){
		message("Getting Cached Data")
		return(inverse)
		## returns the matrix value from cache
	  }
	  ## if it does not exist before then it creates 
	  data <- x$get()
	  ## solve is the basic R function which creates the inverse of the matrix
	  inverse<-solve(data)
	  x$setInverse(inverse)
	  ## sets the value of inverse for the first time
	  
	  inverse
        ## Return a matrix that is the inverse of 'x'
}
