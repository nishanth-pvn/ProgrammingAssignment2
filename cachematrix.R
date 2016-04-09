## Programming Assignment - 2 (Caching the inverse of a matrix) (Week - 3)
## 		Matrix Inversion is well known for its computation complexity,
## 		here we do caching the inverse of matrix so that it will not
## 		be computed repeatedly. Below two functions assists in creating
##		special object that stores the matrix and caches its inverse.

## makeCacheMatrix() - Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) 
				{
                x <<- y
                inv <<- NULL
				}
		get <- function() x
				setinverse <- function(inverse) inv <<- inverse
				getinverse <- function() inv
		list(set = set, 
			 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## catchSolve() - Computes inverse of special matrix returned by makeCacheMatrix above. 
## If the inverse is already calculated then the cacheSolve should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting Cached Data")
                return(inv)
        }
        invmatrix <- x$get()
        inv <- solve(invmatrix, ...)
        x$setinverse(inv)
        inv
}
