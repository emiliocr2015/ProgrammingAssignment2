## The following functions compute the inverse of a  matrix (the matrix should be squared 
## and invertible) and cache the result. If it has already been calculated, the stored data 
## will be returned, instead of repeating the computation of the inverse matrix.
## This is useful to reduce computing time when we are working with large matrices.


## The function 'makeCacheMatrix' transforms a matrix into a list object with 4 elements
## that can cache its inverse.
## The returned object should be used as an input in the second function, 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    setmat<-function(y){ ## set the matrix
        x<<-y
        inv<<-NULL
    }
    getmat<-function(){ ## get the matrix
        x
    }
    setinv<-function(inverse){ ## set the inverse
        inv<<-inverse
    }
    getinv <- function() { ## get the inverse
        inv
    }
    list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)
}


## The second function 'cacheSolve' returns the inverse of the initial matrix, using the output
## of the function above as an input. In case this inverse hasn't been solved yet, the inverse will 
## be computed and cached. If it has been already calculated, the function returns the stored data
## and notifies it with a message.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) { ## check if the inverse  has been already computed. 
        message("getting cached data") ## If it has, returns the stored data.
        return(inv) 
    } else { ## otherwise, compute the inverse and cache it.
    data <- x$getmat()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    }
}
