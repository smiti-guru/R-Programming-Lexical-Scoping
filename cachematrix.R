@@ -0,0 +1,38 @@
  ## Caching the invrerse of a Matrix:
  ## Below are two functions that are used to create an object that 
  ## stores a matrix and caches its invrerse.
  
  ## This function creates a special "matrix" object that can cache its invrerse.
  
  makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) 
      x <<- y
    invr <<- NULL
    
    get <- function() x
    setinvrmtrx <- function(invrerse) invr <<- invrerse
    getinvrmatrx <- function() invr
    list(set = set,
         get = get,
         setinvrmtrx = setinvrmtrx,
         getinvrmatrx = getinvrmatrx)
  }
  
  
  ## This function computes the invrerse of the special "matrix" created by 
  ## makeCacheMatrix above. If the invrerse has already been calculated (and the 
  ## matrix has not changed), then it should retrieve the invrerse from the cache.
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the invrerse of 'x'
    invr <- x$getinvrmatrx()
    if (!is.null(invr)) {
      message("getting cached data")
      return(invr)
    }
    mat <- x$get()
    invr <- solve(mat, ...)
    x$setinvrmtrx(invr)
    invr
  }
 
