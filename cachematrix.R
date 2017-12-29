###This script contains two functions:
        #makeCacheMatrix:       initializes a 'special' matrix
        #cacheSolve:            retrieves the cached inverse (if present). If not present, 
        #                       it is calculated and cached.


#makeCacheMatrix creates a list of four functions:
        #set: stores the original matrix and initializes the inverse
        #getOriginal: retrieves original matrix
        #setInverse: caches the calculated inverse matrix
        #getInverse: retrieves the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        getOriginal <- function() x
        set <- function(orig) {
                original <<- orig
                inverse <<- NULL
        }
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        
        #list containing the four functions
        list( set = set, 
              getOriginal = getOriginal,
              setInverse = setInverse,
              getInverse = getInverse
             )
}


## cacheSolve function retrieves the inverse of the original
## matrix if it has been cached. If not cached, the inverse
## is calculated and cached.
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        # Determine if inverse is already cached. If so, retrieve it.
        if(!is.null(inverse)) {
                message("Retrieving inverse from cache")
                return(inverse)
        }
        #retrieve original matrix
        data <- x$getOriginal()
        
        #calculate inverse matrix using the 'solve' function
        inverse <- solve(data)
        
        #cache the calculated inverse matrix
        x$setInverse(inverse)
        return(inverse)
}

##These are examples
# x <- matrix(c(4,2,7,6), ncol = 2, nrow = 2)
# m <- makeCacheMatrix(x)
# cacheSolve(m)
# 
# x2 <- matrix(c(1,0,5,2,1,6,3,5,0), ncol = 3, nrow = 3)
# m2 <- makeCacheMatrix(x2)
# cacheSolve(m2)
