## Put comments here that give an overall description of what your
## functions do

## Create object inverse matrix
makeCacheMatrix <- function(x = matrix()) {
matrix <- NULL              
set <- function(y) {					#write matrix	
x <<- y
matrix <<- NULL
}
get <- function() x					#read matrix
setsolve <- function(solve) matrix <<- solve		#write inverse
getsolve <- function() matrix				#read inverse
list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
matrix <- x$getsolve()
if(!is.null(matrix)) {                          
return(matrix)						#return cache inverse
}
data <- x$get()
maxtrix <- solve(data)					#calculate inverse
x$setsolve(matrix)					#save inverse
matrix}
