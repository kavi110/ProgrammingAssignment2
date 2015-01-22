## To save time and workload of a program this code calculates
##  the inverse of a given matrix and caches it and will recover  
## the cached inverse matrix when needed

## makecacheMatrix function calculates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL				
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

 	## calculating the inverse of the matrix  and returning in a list

        get <- function() x
        setinverse <- function(solve) m <<- solve	
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cachesolve function caches the inverse of matirix alculated from 
## makecachematix function, while checking if the matrix is inverded 
## if not it will calculate the inverse of the matrix.


cacheSolve <- function(x, ...) {
	## checking if the inverse matrix has cached
        

 	  m <- x$getinverse()	   
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	##Return a matrix that is the inverse of 'x'

        matrix <- x$get()          
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}

