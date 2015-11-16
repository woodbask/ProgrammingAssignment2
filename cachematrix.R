#This function creates a special matrix that is a list containing a function
#to do the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	# initialize the environment object 'm' for the matrix inverse
      	m <- NULL

	# sets the environment object 'x' with the matrix passed in argument.
	# initializes the environment object 'm'.
      	set <- function(y) {
      		x <<- y
      		m <<- NULL
        }

	# returns the matrix from the environment object 'x'
   	get <- function() x

	# sets the environment object 'm' with the inverse matrix passed 
	# in argument.
    	setInverse <- function(solve) m <<- solve

	# returns the inverse matrix from the environment object 'm'
   	getInverse <- function() m

	# create a list of the functions
    	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned  
#by makeCacheMatrix.  If the inverse has already been calculated
#and the matrix has not changed, then this function retrieves the inverse 
#from the cache. 

cacheSolve <- function(x, ...) {
	# gets the inverse of the matrix
      	m <- x$getInverse()

	# if the inverse matrix is not null, then already cached. Return
	# the cached inverse matrix with a message to indicate cached data.
      	if(!is.null(m)) {
      	message("getting cached data")
            return(m)
    	}

	# if not already cached, get the matrix and inverse it.
	# NOTE: the matrix to be inversed, must be invertible.
      	data <- x$get()
      	m <- solve(data, ...)

	# set and return the inverse matrix.
      	x$setInverse(m)
      	m						
}
