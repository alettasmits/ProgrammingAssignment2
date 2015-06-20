## CacheMatrix will create a variable for the inverseMatrix
## And return its value if asked for it. If no inverse matrix is stored it will return NULL
## If an inverse matrix is calculated it will cache it (setInverse)

cacheMatrix <- function(x = matrix()) {
  ##first a variable is created that will contain the inverse vector if present
  ##as long as there is no inverse vector it will be NULL
  inverseMatrix <-NULL
  ##the set function: will assign the matrix to x. 
  ##And since it is a new matrix the inverseMatrix needs to be 
  ##set to NULL; its inverse needs to be calculated still
  set <-function(y) {
      x<<-y
      inverseMatrix <-NULL
    }
  ##the get function will return the value that was earlier assigned to x  
  get <- function(){
      x
  }
  ##setInverse will give x the value of the inverted matrix (calculated elsewhere)
  ##inverseMatrix has a value now and will not return NULL when tested
  setInverse<-function(solvedMatrix) {
    inverseMatrix<<-solvedMatrix
  }
  ##getInverse will return the inverseMatrix; it will return NULL if it has not been calculated
  getInverse<-function() {
    inverseMatrix
  }
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ##give inverseMatrix a value. If it has not been calculated yet, it will
  ##not be cached and the value returned is NULL
  inverseMatrix <-x$getInverse()
  ##test for NULL: if not, return value and state that it was cached;
  if(!is.null(inverseMatrix)) {
    message("getting inverse matrix from cache")
    return(inverseMatrix)
  } 
  ##if inverseMatrix is NULL then continue calculation
  else {
    inverseMatrix <- solve(x$get())
    ##and commit the inverseMatrix to the cache
    x$setInverse(inverseMatrix)
    ##and print the inverseMatrix
    inverseMatrix
  }
}
