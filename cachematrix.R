## These functions allow a user to set the value of a matrix and calculate its inverse,
## with the ability to use a cached inverse result rather than re-calculating, if applicable

## makeCacheMatrix creates a list containing a matrix x and the options to set/get it and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL	
	}
	get <- function() x
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes returns the cached inverse of matrix x if available; otherwise it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
