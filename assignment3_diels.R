
#### Peer-graded Assignment: Programming Assignment 2: Lexical Scoping ####
## by Juri Diels

# This pair of functions cache the inverse of a matrix and gives it out regardless of wether it has already been calculated and stored or not.



# the makeCacheMatrix function takes the input matrix and hands out a list of 4 functions.
      # the first function sets the matrix
      # the second function gets the matrix
      # the third function sets the values of the inverse of the function
      # the fourth function gets the values of the inverse of the function

#makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) { 
            x <<- y
            mx <<- NULL
      }
      get <- function() x 
      setinv <- function(inv) mx <<- inv 
      getinv <- function() mx
      list(set = set, get = get, 
           setinv = setinv,
           getinv = getinv)
}


# the cacheSolve function computes the inverse of the matrix given to an object 
# by makeCacheMatriox function. First it is checked, if there is already a calculated inverse
# of the matrix. In this case, it gets the inverse from the cache and skips
# computing. If this is not the case, the function calculates the inverse of
# the matrix (data) and sets the value of the inverse matrix using the setinv function

#cacheSolve function
cacheSolve <- function(x, ...) {
      mx <- x$getinv()
      if(!is.null(mx)) {
            message("getting cached matrix")
            return(mx)
      }
      data <- x$get()
      mx <- t(data, ...)
      x$setinv(mx)
      mx
}


#test
mtrx <- makeCacheMatrix(matrix(1:12, 3, 4))
cacheSolve(mtrx)