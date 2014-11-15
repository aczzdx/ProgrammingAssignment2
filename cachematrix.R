## cachematrix.R
## An implement of caching the inverse of a matrix 

##  makeCacheMatrix: a function that return a object which can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	my_inv <- NULL
	
	set <- function(y){
		x <<- y
		my_inv <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setinv <- function(inv) {
		my_inv <<- inv
	}
	
	getinv <- function() {
		my_inv
	}
	
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve : return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

		inv <- x$getinv()
		if(!is.null(inv)){
			return(inv)
		}
		
		my_data <- x$get()
		inv <- solve(my_data, ...)
		x$setinv(inv)
		
		inv
		
}
