## Functions calculate and store the inverse of a matrix
## 2017-05-24       M. Welling

## function makeCacheMatrix stores a matrix and it's inverse in cache
## The function checks if the inverse can be determined. 
## If not a message indicating the reason is given
##   - The matrix must be square
##   - The determinant of the matrix must be unequal to 0

makeCacheMatrix <- function(x = matrix()) {
	# check that x is square
	if (nrow(x) != ncol(x)) {
		print("Matrix must be square")
		return()
	}
	# check that determinant of matrix is unequal to 0
	if (det(x, method = c("qr","eigenvalues")) == 0) {
		print("Matrix must have determinant != 0")
		return()
	}

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	     setinverse = setinverse,
              getinverse = getinverse)
}


## Function cacheSolve determines the inverse of a matrix

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
