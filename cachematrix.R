## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (x = matrix(nrow =0, ncol =0)) {
        minv <- matrix(nrow =0, ncol =0)
        set <- function(y) {
			if (!is.matrix(y))
			{
				end("argument not a matrix, assignment rejected")
			}
			x <<- y 
			minv <<- matrix(nrow =0, ncol =0)		    		
        }
        get <- function() x
        setinverse <- function(minverse) minv  <<- minverse
        getinverse <- function() minv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
        if(length(minv) > 0) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv

}
