## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix saves the inverse of a matrix
#Why we want to save inverse? Because its one of the costliest operations
#If matrix is pretty huge we would like to avoid computing inverses everytime we need it
#So? Keep it in Cache and retrieve it when needed!
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y){
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInv <- function(inv){
    invX <<- inv
  }
  getInv <- function() invX
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## Write a short comment describing this function
#The following function does most of the work though
#it checks if marix inverse is available in cache
#if its not then it calculates the cache and saves it
#where does it save? In the environment of the makeCacheMatrix function using <<- operator
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- x$getInv()
  if(!is.null(invX)){
    message("getting cached data")
    return(invX)
  }
  matX <- x$get()
  invX <- solve(matX)
  x$setInv(invX)
  return(invX)
}
