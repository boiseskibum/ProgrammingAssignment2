## These functions are utilized to reduce the cost of cmoputation required for a 
## Matrix Inversion if it is done repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        
        #create the functions
        set <- function(y) {
                x <<- y
                invertedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invertedMatrix <<- inverse
        getInverse <- function() invertedMatrix
        
        #this puts the functions into a list and returns them
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache instead of doing it again.

cacheSolve <- function(x, ...) {
  
        
        invertedMatrix <- x$getInverse()
        
        # return the value if it is already cached
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        
        matrixData <- x$get()
        
        ## added some error handling I saw from another solution
        tryCatch( {
                invertedMatrix <- solve(matrixData, ...)
        },
        error = function(e) {
                message("SRT Error: ")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("SRT Warning: ")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setInverse(invertedMatrix)
        } )        
                
 
        ## Return a matrix that is the inverse of 'x'
        invertedMatrix

  
}
