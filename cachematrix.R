## Note that this is more description than I would usually include or expect to
## see. However, since a lot of the students here are beginners, I thought a very
## detailed walk-through might be beneficial in their learning.

## These two functions, working in tandem, allow a user to create a matrix and
## calculate it's inverse,  then store that value in a cache so it can be called
## later, possibly many times, without having to recalculate the inverse again
## and again. The first function, makeCacheMatrix, sets the matrix values and
## defines necessary functions, and later creates the storage environment for a
## calculated inverse. The second function, cacheSolve, either calculates the
## inverse and then stores it back within the makeCacheMatrix environment, or if
## already stored, simply prints the cached value.

## This first function sets up an environment that contains variables corresponding
## to a matrix, and several functions. The set function can be used on it's own
## to update the value of the matrix, and the get function on it's own to see the
## value of the non-inverse matrix. The functions get, setinverse, and getinverse
## all are used later in the cacheSolve function. One could use the getinverse
## function on it's own to see the value of an inversed matrix, but only after
## running cacheSolve, and the answer would be the same as running cacheSolve. 
## One could tehcnicaly use the setinverse function on it's own, but that would
## not be advisable as that is meant to be calculated as an inverse of a matrix,
## not just input manually.

makeCacheMatrix <- function(x = matrix()) {
     # This initiates i as an empty object.
     i <- NULL
     # This creates a function that can be called later whereby a user can adjust
     # the value of a matrix without having to redo the entire makeCacheMatrix
     # function. It sets the value of y to x witin the parent entirement, and
     # resets i in  the parent environment to be empty/NULL.
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     # This creates a function that returns just the value of x (the matrix as
     # determined by either an original makeCacheMatrix value, or a x$set value).
     get <- function() x
     # This creates a function that replaces the empty i object in the parent
     # environment with the value of the function's arguement (inverse). This
     # inverse argument will be filled in near the end of the cacheSolve function.
     setinverse <- function(inverse)
                    i <<- inverse
     # This function simply returns the value of i, previously NULL but set to a
     # particular matrix by the previous setinverse function.
     getinverse <- function() i
     # This puts each of the above functions into a list and assigns them a name,
     # allowing the use of a $ operator in the cacheSolve function.
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}

## This second function uses functions defined above to first check if an inversed
## matrix has already been calculated and stored. If it has, it simply returns
## that value. If not, it grabs matrix data, calucates the inverse, stores that
## inversed matrix in cached memory, and prints the inversed matrix.

cacheSolve <- function(x, ...) {
     # This defines i2 as the value found by running getinverse on a particular
     # object x. If it finds that it is not NULL (ie, does contain a value),
     # that means that the matrix inverse has already been calculated and is
     # waiting in cache, so a brief message is printed, and then it returns the
     # value of the cached matrix, with no calculation necessary.
     i2 <- x$getinverse()
          if (!is.null(i2)) {
               message("getting cached data")
               return(i2)
          }
          # If i2 IS empty/NULL, then it grabs a non-yet-inversed matrix, as found
          # by running the x$get function (found within the environment of x).
          data <- x$get()
          # It then takes that matrix, calculates the inverse using solve, and
          # defines it as the i2 variable.
          i2 <- solve(data, ...)
          # It then takes that calculated matrix (i2) and stores it in cached
          # memory by running the x$setinverse function (found within the
          # environment of x).
          x$setinverse(i2)
          # Finally, it returns a matrix that is the inverse of 'x'.
          i2
}
