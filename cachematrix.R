## This file defines two functions
##  1. makeCacheMatrix() : this function is used to create a cached matrix, 
##                         which stores matrix and its cached inverse which 
##                         can be calculated using following function
##
##  2.  cacheSolve()  : this function is used to calculate inverse of matrix 'x' and stores   
##                      the result in the environment which 'x' belongs to


## This function creates a matrix, it can cache its inverse
## using the function cacheSolve() defined below
## it exposes following functions 
##    set() to set the new matrix, in a variable x
##    get() to get the cached matrix i.e.  x
##    setinv() to set new inverse to the matrix, in variable inv
##    getinv() to get the cached inverse (inv) of stored matrix (x)
makeCacheMatrix <- function(x = matrix()) {
  
  #check if the matrix supplied is degenerate/singular or default/unitialized
  isDet <- function(a1=x) {
    a<-det(a1) 
    return ( a!=0||is.na(a))
    
  }  
  if(!isDet()){
    print(cat("matrix is singular/uninitalized","\n" 
              ,"please use set() to initialize with singular matrix","\n",
              "initializing 1x1 matrix with NA"));
    x<-matrix()
  }
  
  inv <- NULL
  
  set <- function(y) {
    
    if(!isDet(y)){
      print(cat("matrix is singular/uninitalized","\n" 
                ,"please use set() to initialize with singular matrix","\n",
                "initializing 1x1 matrix with NA"));
      x<<-matrix()
      return() 
    }
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invr) inv <<- invr
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
  
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x', first try it from cache of 'x' 
## if null then calculates inverse of 'x' and stores in cache
cacheSolve <- function(x, ...) {
  
  invm <- x$getinv()
  if(!is.null(invm)) {
    print("returning cached inverse")
    return(invm)
  }
  
  matr <-x$get()
  invm <- solve(matr)
  x$setinv(invm)
  invm
}
