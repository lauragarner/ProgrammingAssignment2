## These functions work together to set a cache value for the inverse of
## a matrix. It uses lexical scoping to allow the inverse value to be called 
## from outside the function, and the inverse retrieved from the cache if 
## relevant. 

# This function is used to provide list values for the given matrix and
# cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
	# Set the inverse object to be null, to clear the previous value if
	# a new input object is entered (x) 
	inv<- NULL
	
	#Set the value of matrix 'x'
	set<- function(y) {
		x<<- y
		inv<<- NULL
	}

	#Return the value of matrix 'x'
	get<- function() x	
	
	#Set the inverse of matrix 'x' and cache it
	setinv<- function(solve) inv <<- solve
	
	#Return the value of the inverse of matrix 'x'
	getinv<- function() inv
	
	#Set the names of the object matrix so they can be called
	list(set = set, 
		get = get, 
		getinv = getinv, 
		setinv = setinv)
}


# This function uses list names previously defined in the makeCacheMatrix
# function. If the x object returns a NULL inverse, then the inverse is
# calculated and set to the cache.

cacheSolve <- function(x, ...) {
	#Check if there is a value for 'inv'
	inv<- x$getinv()

	#If there is a value, get it from the cache
	if(!is.null(inv)) {
		message ("Getting cached data")
		return(inv)
	}

	#If the value is NULL, then calculate the inverse and set it to cache
	m1<- x$get()
	inv<- solve(m1, ...)
	x$setinv(inv)
	
      ## Return a matrix that is the inverse of 'x'
	inv
}
