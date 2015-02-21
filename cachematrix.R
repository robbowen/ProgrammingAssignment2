## Put comments here that give an overall description of what your
## functions do

## Creates a vector that allows one to set, get, setinverse and getinverse of a matrix
## It caches the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to return the inverse of a square matrix. 
## Will check to see if the inverse was previously cached and will return it if present
## There is no error handling for non-square matrices.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}
