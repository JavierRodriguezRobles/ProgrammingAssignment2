## Two functions that achieve the objective of this exercise.
## Create a matrix and store it in cache
## Return an inverse of the first matrix

## The functionality of makeCacheMatrix is:
## setMatrix: Set a matrix
## getMatrix: Return the originalMatrix
## setInverse: Set the inverse matrix
## getInverse: Return the Inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {

	invMatrix<-NULL
	setMatrix<- function(y) {
		x<<-y
		invMatrix<<-NULL
	}
	getMatrix<- function() x
	setInverse<- function(fInverse) invMatrix<<- fInverse
	getInverse<- function() invMatrix

	list(setMatrix = setMatrix, getMatrix = getMatrix,
                 setInverse = setInverse,
                 getInverse = getInverse)

}


## cacheSolve function use the previous function to get and set the Matrix,
## and, of course, calculate the inverse Matrix of the original Matrix using
## the solve() function.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x' (if exist)
	invMatrix<-x$getInverse()

	if(!is.null(invMatrix)){
		message("Obtaining from the cache")
		return(invMatrix)
	}
	data<- x$getMatrix()
	invMatrix<- solve(data, ...)
	x$setInverse(invMatrix)
	invMatrix


}
