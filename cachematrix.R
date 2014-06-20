## The functions below achieves the following
##  makeCacheMatrix : This function is responsible to provide a list of other functions that
##                    would allow the user to store a matrix, retrieve the matrix, store the inverse of the matrix 
##                    into the cache and retrieve the inverse from the cache
##
##  cacheSolve      : This function would take in a matrix. This matrix would be created and retrieved via the functions in
##                    in the list of functions provided by the makeCacheMatrix function. Then this function would compute
##                    the inverse of this matrix and store it in the cache by utilizing the functions provided by the 
##                    makeCacheMatrix. If such a matrix is passed an input, whose inverse is already computed, then the 
##                    inverse is returned from the cache itself.


## This function will return a list of the following functions
##  setMatrix : Function to store a matrix
##  getMatrix : Function to retrieve the matrix
##  setCache  : Function to store the inverse of the matrix into the cache
##              (in the memory so that it can be fetched from the cache, when required without having to compute again, provided the data hasn't changed) 
##  getCache  : Function to retrieve the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) 
{ 
    m <- NULL
    setMatrix <- function(y) 
    {
      x <<- y
      m <<- NULL
    }
    getMatrix <- function() 
                  x
    setCache <- function(cacheMatrix) 
                  m <<- cacheMatrix
    getCache <- function() 
                  m
    list(
          setMatrix = setMatrix, 
          getMatrix = getMatrix,
          setCache = setCache,
          getCache = getCache
        )
}


## This function would be computing the inverse of the matrix stored. This would be using the above function's returned list of functions to either compute the inverse
## of the matrix and store it in the cache or retrieve the inverse of the matrix from cache if it has already been calculated. 

cacheSolve <- function(x, ...) 
{  
  m <- x$getCache() ## first see if the inverse is already stored in the cache or not.
  if(!is.null(m)) ## if avaialble in the cache
  {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix() ## get the matrix stored 
  m <- solve(data, ...) ## compute the inverse of the matrix
  x$setCache(m) ## set the cache
  m ## return the Inverse of the matrix   
}
