## makeCacheMatrix caches the inverse of a matrix
## cacheSolve finds/saves the inverse using makeCacheMatrix

## The following function takes a square numeric matrix as input.
## It returns a list of functions, that help calculate and cache the inverse of the matrix.
## The list of functions include get, set, getinverse and setinverse.
## get function returns the matrix.
## set function sets the matrix and resets the inverse matrix.
## getinverse returns the inverse.
## setinverse sets the inverse.

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


## cacheSolve takes a makeCacheMatrix as input.
## It tries to find the inverse using getinverse function.
## If it finds the inverse, the value is returned.
## It if doesn't find the inverse, it calculates the inverse using the solve function; set it in the makeCacheMatrix instance; and returns the inverse.

cacheSolve <- function(x, ...) {
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
