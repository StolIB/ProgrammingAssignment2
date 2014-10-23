## Create object for inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL     
	nochange <- F         
	set <- function(y) {					#write matrix	
		if (!mateval(y)) {
			x <<- y
			matrix <<- NULL
			nochange <<- F
		}
	}
	get <- function() x					#read matrix
	setsolve <- function(solve) { 				#write inverse
		matrix <<- solve
		nochange <<- T
	}		
	getsolve <- function() matrix				#read inverse
	getchange <- function() nochange			#read change
	matequal <- function(y) 				#compare matrix
        	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
 	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	matrix <- x$getsolve()
	if(!is.null(matrix) && x$getchange) {                          
		return(matrix)					#return cache inverse
	}
	data <- x$get()
	maxtrix <- solve(data)					#calculate inverse
	x$setsolve(matrix)					#save inverse
	matrix
}
