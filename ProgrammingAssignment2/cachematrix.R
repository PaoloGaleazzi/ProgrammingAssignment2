## Put comments here that give an overall description of what your
## functions do: The first function makeCacheMatrix is given 
## a matrix x as input
## and outputs a list of 4 functions. The second function 
## uses these 4 functions to compute the inverse of a new matrix or
## to output the inverse if already cached

## Write a short comment describing this function: takes a matrix and
## outputs 4 functions setting and calling the matrix and its inverse when computed 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set.mat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get.mat <- function() x
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  list(set.mat = set.mat, get.mat = get.mat, 
       set.inv = set.inv, get.inv = get.inv)
}


## Write a short comment describing this function: it computes
## the inverse of a matrix or it outputs the cached inverse if 
## already computed

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- z$get.inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  } else {
    mat <- z$get.mat()
    inv <- solve(mat)
    z$set.inv(inv)
    inv
  }
}
