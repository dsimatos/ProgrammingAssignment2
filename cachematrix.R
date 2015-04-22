## * Function makeCacheMatrix() creates a special "matrix" object that
## can cache its inverse
## * Function cacheSolve() acts like solve() to compute the inverse of a
## matrix produced by makeCacheMatrix(). It either returns the cached
## inverse matrix, or computes and returns it in case no cache inverse
## matrix exist 

## Function makeCacheMatrix() creates a special "matrix", 
## which is really a list containing a function to
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse matrix
## 3.get the inverse matrix
##
## Usage : var1 <- makeCacheMatrix(var2)
## where var2 is an invertible square matrix
##
## Specific information on every statement follows...

makeCacheMatrix <- function(x = matrix()) {
   ## Check if parameter is a matrix
   if (is.matrix(x)) {
      ## Check if parameter is a square matrix
      x_rows<-attr(x,"dim")[1]
      x_cols<-attr(x,"dim")[2]
      if (x_rows == x_cols) {
         ## Check for numeric values in matrix
         for (i in x) {
            if(!is.numeric(i) || is.na(i)) {
               message("A not numeric value found in matrix")
               return(NULL)
            }
         }
         ## Initialize inverse matrix
         inverse_m <- matrix()
         ## Define the function that "sets" the matrix
         set <- function(y) {
            x <<- y
            inverse_m <<- matrix()
         }
         ## Define the function that "gets" the matrix
         get <- function() x
         ## Define the function that "sets" the inverse matrix
         set_inv <- function(inv_m) inverse_m <<- inv_m
         ## Define the function that "gets" the inverse matrix
         get_inv <- function() inverse_m
         ## Give names to the elements of the list
         list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
      ## In case parameter is not a square matrix,
      } else {
         ## inform the user for proper use...
         message ("Usage : var1 <- makeCacheMatrix(var2), where var2 is a square invertible matrix")
         ## ... and return nothing from the function
         return(NULL)
      }
   ## In case of wrong parameter (not a matrix),
   } else {
      ## inform the user for proper use...
      message ("Usage : var1 <- makeCacheMatrix(var2), where var2 is a square invertible matrix")
      ## ... and return nothing from the function
      return(NULL)
   }
}


## Function cacheSolve() computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache
##
## Usage : cacheSolve(var1, ...) 
## where var1 is an object produced by makeCacheMatrix() function
##
## Specific information on every statement follows...

cacheSolve <- function(x, ...) {
   ## Check if parameter is a special "matrix" object produced by
   ## makeCacheMatrix(). The condition "length(names(x))>=4" stands for
   ## the case that someone introduces more elements in the list that keeps
   ## the special "matrix" object
   if (is.list(x) && 
          length(names(x))>=4 && 
          names(x)[1]=="set" && names(x)[2]=="get" && 
          names(x)[3]=="set_inv" && names(x)[4]=="get_inv") {
      ## Get whatever exist as inverse matrix in the "matrix" object
      inverse_matrix<-x$get_inv()
      ## Check that a cached inverse matrix really exist
      if (!identical(inverse_matrix, matrix())) {
         ## Inform the user for the action (of getting cashed inverse matrix)
         message("getting cached data")
         ## Return the invrse matrix
         return(inverse_matrix)
      ## Cached inverse matrix do not exist
      } else {
         ## Get the original matrix
         data <- x$get()
         ## Compute the inverse matrix
         inverse_matrix <- solve(data, ...)
         ## Cache the inverse matrix in the object
         x$set_inv(inverse_matrix)
         ## Return the inverse matrix
         inverse_matrix
      }
   ## In case of wrong parameter (not a "matrix" object),
   } else {
      ## inform the user for proper use...
      message ("Usage : cacheSolve(var1, ...), where var1 is an object produced by makeCacheMatrix() function")
      ## ... and return nothing from the function
      return(NULL)
   }
}
