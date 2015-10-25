## This pair of functions is used to define a system for caching matrix
## inverses.  The first defines a data structure to store a matrix with
## it's inverse (or NULL).  The second is the function that should be used
## to access the inverse, respecting the cached state.

## Function returns an List of functions for accessing x and i closures.
## x is intended to be a square matrix and i it's (cached) inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get,
	     setinverse = setinverse, getinverse = getinverse)
}


## Function to either retrieve a cached matrix inverse from an
## object returned by the above function, or solve for the inverse,
## returning it, and caching the inverse as a side effect.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
			## Using a cached inverse
			return(i)
		}
		
		## Otherwise generating and caching an inverse
		m <- x$get()       ## m is the underlying matrix data found in x
		i <- solve(m, ...) ## note that we pass any additional arguments to solve
		
		x$setinverse(i)
		
		return(i)
		
}
