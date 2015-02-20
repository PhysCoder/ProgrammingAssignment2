## The makeCacheMatrix returns a list that builds a special matrix object which stores both the matrix and it's inverse
## if it has been calculated

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  data <- x
  set <- function(y){
    data <<- y
    inv <<- NULL
  }
  get <- function(){ data }
  setInv <- function(m){ inv <<- m }
  getInv <- function(){ inv }
  list( set=set, get=get, setInv=setInv, getInv=getInv )
}


## computes the inverse of the special "matrix" from makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    message('Getting cached inverse...')
    inv
  }
  inv <- solve(x$get())
  x$setInv(inv)
  inv
}
