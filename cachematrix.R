## Below are two functions that are used to create a special object that calculates and cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
# 1) setMatrix - set the value of a matrix
# 2) getMatrix - get the value of a matrix
# 3) setCache - set the cache value
# 4) getCache - get the cache value

makeCacheMatrix <- function(x = matrix()) {
		cache <- NULL
		setMatrix <- function(y){
			x <<- y
			cache <<- NULL
		}
		getMatrix <- function() {
			x
		}
		setCache <- function(solve) {
			cache <<- solve
		}
		getCache <- function() {
			cache
		}
		list(setMatrix = setMatrix, getMatrix = getMatrix, setCache = setCache, getCache = getCache)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getCache()
        # if it exists, return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # If not, then get the matrix, calculate it's inverse and store it in the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setCache(inverse)
        
        # return the inverse
        inverse
}
