## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        cache <- x$getInverse()
        
        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(cache)) {
                message("getting cached data")
                
                # display matrix in console
                return(cache)
        }
        
        # create matrix since it does not exist
        matrix <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cache)
        } )
        
        # display matrix in console
        return (cache)
}


####Test, if it works

a <- makeCacheMatrix()     
a$set(matrix(1:4, 2, 2))   
cacheSolve(a)

c <- makeCacheMatrix()     
c$set(matrix(c(1, 2,
               2, 3), nrow=2, byrow=TRUE))
cacheSolve(c)


d <- makeCacheMatrix()     
d$set(matrix(c(1, 2, 0,
               2, 4, 1,
               2, 1, 0), nrow=3, byrow=TRUE))
cacheSolve(d)
