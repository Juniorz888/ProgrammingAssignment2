## These functions are used in getting the inverse of a matrix 
## This will be used in long, repeating process to make the computations fast

## This function returns a list of functions and caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL 
    set <- function(new_mat) {                                  ## assigns a new matrix in the parent environment
        x <<- new_mat
        inverse <<- NULL
    }
    get <- function() x                                         ## gets the matrix
    setinverse <- function(inverse) cached_inverse <<- inverse  ## caches the computed inverse 
    getinverse <- function() cached_inverse                     ## gets the cached inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)

}

## This function returns the inverse of a matrix. 
##It checks first if the inverse has been already computed and returns it. 
##If the inverse is not yet computed, the solve() function and returns it.

cacheSolve <- function(x, ...) {
                                                ## Returns a matrix that is the inverse of 'x'
        cached_inverse <- x$getinverse()        ## retrieves the cached inverse
        if(!is.null(cached_inverse)) {          ## checks if the cached inverse is not null and returns the value
            message("getting cached data")
            return(cached_inverse)
        }
        data <- x$get()                         ## gets the new matrix to be inversed
        inverse <- solve(data,...)              ## inverses the matrix
        x$setinverse(inverse)                   ## setting the computed inverse and caches it
        inverse 
}

