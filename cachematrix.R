##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() { x }
	setInverse <- function(inmatrix){
		inverse <<- inmatrix
	}
	getInverse <- function(){inverse}
	list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


##The following function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse))
        {
        	message("Getting cached data")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
