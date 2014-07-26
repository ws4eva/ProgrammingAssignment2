makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## originally stores null value and updates new value when it's called
        x <<- y
        m <<- NULL
    }
    get <- function() x ## gets stored value
    setinverse <- function(solve) m <<- solve ## stores inverse matrix computed by cacheSolve function
    getinverse <- function() m ## calls cached inverse matrix. Initially null
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) { ## this function returns invers matrix - either computed or cached
    m <- x$getinverse() ## gets inverse matrix from matrix defined from makeCacheMatrix function
    if(!is.null(m)) { ## if there is cached data from $getinverse(), it just returns what's stored instead of computing again and send the message
        message("getting cached data")
        return(m)
    }
    data <- x$get() ## if there is no cached data, it computes inverse matrix and stores through $setinverse function
    m <- solve(data)
    x$setinverse(m)
    m
}