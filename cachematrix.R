## The following functions allow to cache the inverse of matrix (if it 
## has already been calculated) instead of computing it repeteadly.

## makeCacheMatrix creates a special "matrix" object

makeCacheMatrix <- function(z = matrix()) {
	inv<-NULL
	setmatrix<-function(y){
		z<<-y
		inv<<-NULL
	}
	getmatrix<-function() z
	setinv<-function(invmat) inv<<-invmat
	getinv<-function() inv
	list(setmatrix=setmatrix,getmatrix=getmatrix,setinv=setinv,getinv=getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(z, ...) {
	inv<-z$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	matrix<-z$getmatrix()
	inv<-solve(matrix,...)
	z$setinv(inv)
	inv
}