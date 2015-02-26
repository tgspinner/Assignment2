## Put comments here that give an overall description of what your 
## functions do 
 

## takes a matrix and caches it in a global space 
 

 makeCacheMatrix <- function(x = matrix()) {

	## initialize xinverse local
      xinverse <- NULL 

      sety <- function(y) {
       x <<- y

	## invitialize xinverse global
       xinverse <<- NULL 
      }

	## get matrix passed to function
      getx <- function() x 

	## assign and swap to invert
      setInverse <- function(inv) xinverse <<- inv 

      getInverse <- function() xinverse

      list(sety = sety, getx = getx,
            setInverse = setInverse,
            getInverse = getInverse)
}

  

 
## This inverts a matrix and does nothing if it finds it already calc'd 

 
 cacheSolve <- function(x, ...) {
      matrix <- x$getInv()
 
	## check to see if matrix is NULL, otherwise, already solved
      if(!is.null(matrix)) { 
       return(matrix)
      }

      ## if NULL, then calc inverse
	data <- x$get()
      matrix <- solve(data)
      x$setInv(matrix) 
 
      matrix 
}
