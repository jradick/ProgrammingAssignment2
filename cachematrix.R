## 
## Coursera
## Data Science Specialization
## "R Programming" Course
## March 2015
##
## Programming Assignment 2, submitted March 22, 2015
##
## 2 R functions, as specified in the assignment:
## 1. makeCacheMatrix: This function creates a special "matrix" object
##    that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special
##    "matrix" returned by makeCacheMatrix above. If the inverse has
##    already been calculated (and the matrix has not changed),
##    then cacheSolve should retrieve the inverse from the cache.
##
## Jeffrey Radick, student
##
## Implementation directly imitates the style and structure
## of the makeVector() and cachemean() examples provided
## in the statement of the assignment problem.
##

##
## makeCacheMatrix() imitates the example function makeVector()
## but dealing with a matrix instead of a vector,
## and computing a value with solve() rather than mean()
##
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
		    x <<- y
		    m <<- NULL
		}
		get <- function() x
		setsolution <- function(solve) m <<- solve
		getsolution <- function() m
		list(set = set, get = get,
			 setsolution = setsolution,
			 getsolution = getsolution)
}

##
## cacheSolve() imitates the example function cachemean()
## but using solve() to do the computation instead of mean()
## and calling the cached result the "solution" instead of "mean".
## Note that when called with only one argument x,
## solve() takes default arguments and compute the inverse of x,
## BUT if extra arguments are supplied (which must make sense to solve()),
## then a more general matrix equation can be solved.
## This isn't intentional, it's just a side-effect of the way it's coded.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolution()
	if (!is.null(m)) {
	   message("getting cached data")
	   return(m)
	}
	data <- x$get()

	m <- solve(data, ...)
	x$setsolution(m)
	m
}
