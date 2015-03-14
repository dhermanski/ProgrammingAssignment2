## cachematrix.R includes two fuctions: "makeCacheMatrix" and "cacheSolve"
##
## "makeCacheMatrix" accepts a matix (assumed to be "invertible") as input
## and creates a special "matrix" object that can cache its inverse.   
##
## "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed) then "cacheSolve"
## retrieves the inverse from cache.

## "makeCacheMatrix" - accpets a matrix (assumed to be "invertible") as input and creates
## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(input_matrix = matrix()) {
	
	stored_inverse <- NULL

	set <- function(y) {
		input_matrix <<- y
		stored_inverse <<- NULL
	}

	get <- function() {
		return (input_matrix)
	}

	setinverse <- function(sent_replacement_inverse) {
			stored_inverse <<- sent_replacement_inverse
	}

	getinverse <- function() {
		return (stored_inverse)
	}

	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## "cacheSolve" computes and returns the inverse of the special "matrix" returned by 
## "makeCacheMatrix".  If the inverse has already been cacluated then "cacheSolve" simply 
## returns the inverse from cache.

cacheSolve <- function(made_matrix,...) {

	local_inverse  <- made_matrix$getinverse()
	
	if (!is.null(local_inverse)) {
		message("getting cached data")
		return(local_inverse)
	}
	else {
		local_matrix <- made_matrix$get()
		local_inverse <- solve(local_matrix, ...)
		made_matrix$setinverse(local_inverse)
		return(local_inverse)
		}
}