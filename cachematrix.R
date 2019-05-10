## Put comments here that give an overall description of what your
## functions do:
###The first function makeCacheMatrix creates several functions ans variablen to store and get a matrix (the matrix is a input parameter of the function)
###The second function cacheSolve requires the return value from makeCacheMatrix. The function checks if the inverse matrix already has been calculated. If not it calculates the inverse and print the result. 
###With the special operator <<-, the values are assigns to to an object in the parent environment. 

## Write a short comment describing this function
###makeCacheMatrix
###This function creates a list of 4 functions with the following goal: 
###1. set(y): Save input argument in x and assign NULL to m
###2. get(): Gets the matrix x
###3. setinverse(): Defines the setter for the inverse matrix  
###4. getinverse(): Defines the getter function for the inverse matrix. 
### Additionally the variable x (matrix) and m (inverse matrix) are created 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Write a short comment describing this function
###function cacheSolve
###The functions calls the getinverse function on the input argument (which is the ouput from makeCacheMatrix -> the list with the 4 functions)
###If there is already a inverse matrix stored, cahceSolve prints the stores matrix and prints "getting cached data". 
###It there is no inverse matrix stored, cacheSolve calculates the inverse and save it into the variable m by calling the function setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
     }
     