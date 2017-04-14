##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse matrix of the matrix
## - get the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
       cm <- NULL
       set <- function(y){
               x <<- y
               cm <<- NULL
               }
       get <- function() x
       setInverse <- function(solve) cm <<- solve
       getInverse <- function() cm
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        }

##Computes the inverse of the matrix created with the above function. 
##However, it first checks to see if the inverse has already been computed. 
##If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it computes the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
 
 cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
    cm <- x$getInverse()
    
    if(!is.null(cm)){
       message("getting cached data")
       return(cm)
       }
    data <- x$get()
    cm <- solve(data)
    x$setInverse(cm)
    cm      
    }
