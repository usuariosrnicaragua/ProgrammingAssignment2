## Put comments here that give an overall description of what your
## functions do
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of inverse of the matrix
#4. Get the value of inverse of the matrix

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" 
#object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x = matrix(), ...) 
{
  mtx <- x$getinverse()
  if(!is.null(mtx)) {
    message("cached data")
    return(mtx)
  }
  dat <- x$get()
  mtx <- solve(dat, ...)
  x$setinverse(mtx)
  mtx
}



makeCacheMatrix <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL     }
  get <- function() x
  setinverse <- function(inverse) mtx <<- inverse
  getinverse <- function() mtx
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
