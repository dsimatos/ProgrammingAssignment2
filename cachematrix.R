## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverse_matrix<-x$get_inverse()
   if (is.matrix(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
   }
}
