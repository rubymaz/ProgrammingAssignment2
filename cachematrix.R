
## Put comments here that give an overall description of what your 
## functions do

## There are two main functions. makeCacheMatrix() and cacheSolve()
## makeCacheMatrix() offers 4 functions to set() and get() a matrix
## and also setinverse() and getinverse() to set and retrieve the
## inverse of the matrix. cacheSolve()'s job is to return the inverse
## of an input matrix. It does so by calling getinverse() first to
## see if an inverse was previously computed and cached, otherwise,
## it computes the inverse and stores it back

## example execution steps:
## a <- makeCacheMatrix(matrix(c(2,1,3,4,5,7,9,10,20),nrow=3,ncol=3,byrow=TRUE))
## cacheSolve(a)

## OR

## a <- makeCacheMatrix()
## a$set(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
## cacheSolve(a)

## Write a short comment describing this function 

## makeCacheMatrix is the overall function that contains 
## 4 functions, set() that sets data for the matrix,
## get() gets the current matrix at any time, setinverse()
## sets the inverse of the matrix and getinverse() gets
## the currently stored inverse

makeCacheMatrix <- function(x = matrix()) {         
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # declaring the list of the 4 functions that can be called by caller
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

## Write a short comment describing this function 

## cacheSolve takes in a matrix and returns its inverse
## it first calls the getinverse() of makeCacheMatrix
## to see if we already have an inverse solved. If yes,
## that inverse value is returned right-away. A not-null
## return value of getinverse() denotes that an inverse
## was previously computed. In case of a null return value
## of getinverse(), this function retrieves the current matrix
## using get() function, then computes the inverse using solve()
## and stores it back into the cache for future use before
## returning the inverse value

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinverse()
  if (!is.null(inv)) {
    ## this denotes that the inverse was previously computed and cached
    message ("using cached inverse data")
    return(inv)
  }
  ## we need to compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  ## storing the computed inverse for future use
  x$setinverse(inv)
  inv
} 
