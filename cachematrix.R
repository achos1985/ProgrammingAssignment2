# First  we set a function that creates a special "matrix" object
# that can cache its inverse. It creates a list
# that contains 4 member functions: set, get, setInv
# and getInv.

makeCacheMatrix <- function ( x = matrix() ) {

	# We cache the inverse of matrix "x"
	# in the variable "xinv".
	# When there's no matrix, it's supposed to be "NULL".
	xinv <- NULL;
	
	# A setter function, we use this to set a matrix 
      # to an object created by makeCacheMatrix function

	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	
	# return the input matrix.
	get <- function() {
		x
	}
	
	# We can directly set the inverse of the matrix.
	setInv <- function(inv) {
		xinv <<- inv
	}
	
	# We can get the inverse of the matrix.
	getInv <- function() {
		xinv
	}

	# return a list that contains these functions	
	
list(	set = set,
			get = get,
			setInv = setInv,
			getInv = getInv
		)
}



# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
	
	# get the inversed matrix from object x

	m <- x$getInv();
	
	
	if( !is.null(m) ) {
		message("getting chached data")
       return(m)
	}
	else {
		data <- x$get()  # if not, we do x$get to get the matrix object
		m <- solve(data)
            x$setInv(m)
            m # return the solved result
	}
