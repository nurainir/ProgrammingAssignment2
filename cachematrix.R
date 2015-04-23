## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates  a list containing a function to
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

	m_inv <- NULL
#    set the value of the matrix
	set <- function(y) {
		x <<- y
		m_inv <<- NULL
	}
#    get the value of the matrix
	get <- function() x
#    set the value of the inverse
	setinverse <- function(inverse) m_inv <<- inverse
#    get the value of the inverse
	getinverse <- function() m_inv
#return list
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The following function calculates the INVERSE of the special MATRIX created with makeCacheMatrix function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	m_inv <- x$getinverse()
	if(!is.null(m_inv)) {
		message("getting cached matrix data.")
		return(m_inv)
	}
	data <- x$get()
	m_inv <- solve(data)
	x$setinverse(m_inv)
	m_inv

        ## Return a matrix that is the inverse of 'x'
}

##RUN
#> a = matrix(rexp(4),2,2)
#> a
#          [,1]       [,2]
#[1,] 2.6636820 0.08219736
#[2,] 0.3228773 1.58119988
#> b = makeCacheMatrix(a)
#> b$get()
#          [,1]       [,2]
#[1,] 2.6636820 0.08219736
#[2,] 0.3228773 1.58119988
#> cacheSolve(b)
#            [,1]        [,2]
#[1,]  0.37780080 -0.01963966
#[2,] -0.07714605  0.63644149

# second run with cache data
#> cacheSolve(b)
#getting cached matrix data.
#            [,1]        [,2]
#[1,]  0.37780080 -0.01963966
#[2,] -0.07714605  0.63644149

