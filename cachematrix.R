## These functions read a matrix which is then solved and cached to avoid
## recalculation of the inverse

## The function here prompts the user to give a matrix which is read and cached
## with a list that with functions that get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the the inverse of the matrix created with the
## function above. If the inverse has already been calculated, cache is searched
## to find the calculated inverse.

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}

