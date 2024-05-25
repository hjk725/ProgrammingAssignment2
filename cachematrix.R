## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function:

## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x #function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv #function to obtain the inverse of the matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { #gets cache data
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) { #checking whether inverse if NULL
    message("getting cached data")
    return(inv) #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...) #calculates inverse value
  x$setInverse(inv)
  inv #returns a matrix that is the inverse of x
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
