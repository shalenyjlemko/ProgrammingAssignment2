## As I understood the assignment, the code does something analogous to creating a 
## custom class in Python, where the makeCacheMatrix is the constructor function
## and the functions it creates are methods available for the object it creates. 

## In case of R the code is making use of closures to emulate described above. When you call 
## makeCacheMatrix, it defines and returns several functions (set, get, setinv, and getinv).
## Each of these functions is a closure. They all retain access to the environment of makeCacheMatrix,
## which includes the variables x (the matrix) and inv (the cached inverse).



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function calculates the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
