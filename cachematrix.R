# These two functions will compute the inverse of a matrix which you can specify
# using the matrix(data, nrow = , ncol= ) function to input in the first function
# as an argument.

## This is the make CacheMatrix function, this will produce a list object that
## contains all the functions specified in the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  inv = matrix()
  set <- function(y){
    x <<- y
    inv <<- matrix()
  }
  get <- function()x
  setinv <- function(solve) inv <<- solve
  getinv <- function()inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This following function wil retrive the inverse martix, if it is solved in 
## the previous function, if not it will compute it now, and substitute back
## to the previous function's subfunction getinv()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.na(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
}
