## makeCachematrix() creates an object that stores the matrix and caches its inverse
## cacheSolve() inverts the matrix of the given object using the cache first

## Creates an object that holds the matrix and caches its inverse
## with methods to get/set them

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Wrapper for matrix inverse that uses cached inverse if any

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get and return cached inverse if any
	inv <- x$getinv()
	if(!is.null(inv)) {
		return(inv)
	}	
	
	## Caclulate, set and return matrix inverse
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
