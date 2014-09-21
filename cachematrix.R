## 
##   Caching the inverse of a matrix 
##     Programming Assignment 2  R Programming
##
##      Makes use of the <<- operator for assigning a value in the environment
##       different then the current environment
## 
## makeCacheMatrix creates a special object that stores a matrix and caches
##                 its inverse   
##     Steps:
##            set the value of the matrix
##            get the value of the matrix
##            set the value of the matrix's inverse
##            get the value of the matrix's inverst

 makeCacheMatrix <- function(x = matrix()) {
             i <- NULL
             set <- function(y) {
                      x <<- y
                      i <<- NULL
                      }
             get <- function() x
             setinv <- function(solve) i <<- solve
             getinv <- function() i
             list(set = set, get = get, 
                        setinv = setinv, getinv = getinv)
            }


## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already 
##             been calculated (and the matrix has not changed), 
##             then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
               i<- x$getinv()
               if(!is.null(i)) {
                  message("getting cached data")
                  return(i)
                 }
              data <- x$get()
              i<- solve(data, ...)
              x$setinv(i)
              i
 
            }

