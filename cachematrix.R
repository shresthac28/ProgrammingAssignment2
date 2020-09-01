## The given function can calculate the inverse of the given matrix.
## The 'makeCacheMatrix' function creates a special "matix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.Write a short comment describing this function
##we assume that the matrix is invertible.
cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}  
        ## Return a matrix that is the inverse of 'x'
