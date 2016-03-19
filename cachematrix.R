## These two functions operate in tandem to take the inverser of a square matrix (e.g mySqrMatrix <- matrix(1:4,2,2).
## makeCacheMatrix takes matrix and caches it. Set this function as a var in the console
## > myCachedMatrix <- makeCacheMatrix(mySqrMatrix)
## You can then call cacheSolve(myCachedMatrix) and the result will be the inverse of mySqrMatrix
## 

## this is the function for cacheing the matrix

makeCacheMatrix <- function(x = matrix()) {
    # m is ultimately what the inverse of the matrix will be set to (in cacheSolve, but only if it is NULL we establish it's existence (and nullness) here
    m <- NULL
    # set is an empty fuction that sets the value of x to whatever is passed for set, for use in cacheSolve, m is also established for use in cacheSolve (still NULL)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get is an empty fuction that just returns x (i.e get(x) will result in the matrix), used to store x in data in cacheSolve
    get <- function() x
    # setsolve stores "solve" in m
    setsolve <- function(solve) m <<- solve
    # getsolve is an empty fuction that returns m
    getsolve <- function() m
    # the list only makes sense if stored in a variable that is passed with cacheSolve
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve uses solve() to return matrix store in cache by makeCacheMatrix
# cacheSolve takes the list created in cachemean

cacheSolve <- function(x, ...) {
    #remember x is the list created in makeCacheMatrix
    m <- x$getsolve()
    # x$getsolve is the 4 element of the list and is NULL because m is NULL in makeCacheMatrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # m is NULL so function moves on
    # recall that get is an empty function on x, so the original matrix is now stored as data
    data <- x$get()
    # execute the inverse of the original matrix, this is the pair to setsolve above
    m <- solve(data, ...)
    # I'm not sure why this line is here, the function works with out it, but I copied it over from the vector example.
    x$setsolve(m)
    # return the inverse of the matrix
    m
}
