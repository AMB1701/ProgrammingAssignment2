## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix provides functions and variables that represent a square
##  matrix and its inverse.  The inverse of a matrix is initially NULL.  Accessor
##  and mutator functions are provided for the original matrix (get() and set(x), 
##  respectively) and the inverse (getInverse() and setInverse(x), respectively).
makeCacheMatrix <- function(mat = matrix()) 
{
  inverse <- NULL
  
  set <- function(x) 
  {
    mat <<- x
    inverse <<- NULL
  }
  
  get <- function() mat
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(mtx, ...) 
{
  solvedInverse <- mtx$getInverse()
  if(!is.null(solvedInverse))
  {
    return(solvedInverse)
  }
  
  mat <- mtx$get()
  solvedInverse <- solve(mat,...)
  mtx$setInverse(solvedInverse)
  solvedInverse
}
