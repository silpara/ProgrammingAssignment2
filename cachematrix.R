  ## Put comments here t###hat give an overall description of what your
## functions do
##The idea is to reduce the computation effort which are really really expensive.
##Calculating matrix inverses is one of them
##Since it is a costly operation consuming lots of computational resources and possible warming up your computer to unpleasant temperatures. You would ideally like to compute it once and save and whenever you need just retrieve the answer calculated from previous effort
##This is exactly what is done in makeCacheMatrix : it saves the cache and cacheSolve which retrieves the inverse from cache and if it is not there, it calculates the inverse and saves it in cache (or simply caches it)


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
