## Put comments here that give an overall description of what your
## functions do
## The functions get a matrix and calculates its inverse, but before doing so, 
## they check if the inversed matrix has already been calculated and is cached. 
## In that case, then gets the inversed matrix from cache. 
## Otherwise, the functions calculate the inverse of the matrix given. 

## Write a short comment describing this function
## makeCacheMatrix gets one argument that is a matrix and creates a 4 elements list, 
## containing 4 functions: set, get, setinv and getinv. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    ##sets de variable inv to null
  ## "set" function gets the value of the argument and assigns it to the free variable x
  ## Then initializes the value of inv to null. We do it again in case the value of 
  ## x is given again, the the value of inv may change if x is different. 
  set <- function  (y) {
    x <<- y 
    inv <<- NULL  ##inv is a free variable here, and searchs for it in parent environment
    ## and assigns NULL value in that environment
    x
  }
  ## "get" function gets the value of x that is kept in memory
  get <- function () {
    x
  }
  ## "setinv" function assigns the value of the argument (solve) to the free variable inv
  setinv <- function (solve) {
    inv<<-solve  ##assigns the value solve to the var inv in the environment where inv is defined and found
    inv
  }  
  ## "getinv" function gets the value of inv that is kept in memory
  getinv <- function() inv
  
  ## creates a list with the values of the four functions, and assigns names to them
  list (set = set, get = get, 
        setinv = setinv, 
        getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve gets the given argument x (which is a list) and checks if the element
## getinv of the list is null. If it is not null, it means it exists in cache
## and gets if from cache and sends a message with that information. 
## If getinv is null, then the function calculates the inverse of the matrix.
## First creates a variable with the value of the element get of the list x, whith is the given matrix. 
## then calculates de inverse of the given matrix and assigns it to the element
## of the list setinv, invoking the function setinv. Then prints a message informing
## about this situation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  matrix.data <- x$get() 
  inv <- solve (matrix.data,...)
  x$setinv(inv) 
  inv
  message("calculating inverse")
  print(inv)
}
