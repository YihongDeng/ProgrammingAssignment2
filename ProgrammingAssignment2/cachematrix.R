## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(matrix){
      x<<-matrix
      i<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) i<<-inverse
    getinverse<-function() i
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-inverse(data,...)
  x$setinverse(m)
  m
}







