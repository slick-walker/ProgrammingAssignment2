## This is a pair of functions which cache the inverse of a matrix and
## will return what is in the cache if the inverse has already been
## computed.

## makeCacheMatrix will create the matrix object which holds the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {

	## Initially set the inverse to null when creating the 
	## cache matrix object.
	inv <- NULL

	## The set function will set a new matrix and reset the
	## inverse to null.
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## The get function will return the stored matrix.
	get <- function() x

	## The set inverse function will set the inverse of the
	## matrix.
	setinv <- function(inverse) inv <<- inverse
	
	## The get inverse function will return the inverse of
	## the matrix.
	getinv <- function() inv

	## Return a list of functions for the cached matrix object.
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

m
## cacheSolve will calculate the inverse of a matrix if it has not
## already been calculated. If the inverse has been calculated, the
## cached inverse will be returned.

cacheSolve <- function(x, ...) {
	## Check to see if the inverse is not null.
	if (!is.null(x$getinv())) {
		## Display a message that there is cached data available
		## and simply return the cached data.
		message("getting cached data")
		return(x$getinv())
	}

	## Get the matrix data from the object.
	data <- x$get()

	## Call the solve function to get the inverse.
	inv <- solve(data)

	## Set the inverse in the object.
	x$setinv(inv)

	## Return the inverse from this function.
	inv
}