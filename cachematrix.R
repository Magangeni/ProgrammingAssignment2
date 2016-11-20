## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The funtion cretes a list that contain functions to
##  a) set the value of a square matrix 
##  b) get the value of the square matrix in a)
##  c) set the value of the inverse of the matrix
##  d) get the value of the invrse of the matrix
makeCacheMatrix <- function(x = matrix(rnorm(16),4)) {
     m <- NULL
     set <- function(y){
         x <<- y
         m <<- NULL
     }
     
     get <- function() x
     setInverse <- function(mInverse) m <<- mInverse
     getInverse <- function() m
     list(set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function
# The function cacheSolve(x,...) computes the inverse of its argument
# In the first call to cachesolve(x,...) the function calls solve() to compute the inverse
# It caches the compute inverse which it returns in subsequent calls
# The function assumes the matrix is invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
