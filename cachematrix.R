## Put comments here that give an overall description of what your
## functions do

## File contains two functions, makeCacheMatrix() and cacheSolve(), together
## the will provide the inverse of a Matrix either from a cache, if one exists,
## or by solving the inverse and cacheing it. They must be used together.


## Write a short comment describing this function

## Use the first function by feeding a matrix to makeCacheMatric()
## to create a list that is used by cacheSolve() 

## Note There are several thousand words explaining these functions on 
## the forums in Coursera, I changed some of the object names to make 
## things a little bit more explicit.
## I hope this doesn't making grading more difficult, but I think it will 
## help me in the future if I ever review this.

makeCacheMatrix <- function(fed_mat = matrix()) {
  in_matrix <- NULL
  set.m <- function(another_mat) {
    fed_mat <<- another_mat 
    in_matrix <<- NULL
  }
  get.m <- function() fed_mat
  set.m.inverse <- function(fed_inverse) in_matrix <<- fed_inverse
  get.m.inverse <- function() in_matrix
  list(set.m = set.m, get.m = get.m,
       set.m.inverse = set.m.inverse,
       get.m.inverse = get.m.inverse)
}

## If you understand the assignment, you probably know this, BUT...
## fed_mat is the input matrix, or you can reset it by calling set.m by itself 
## when another matrix replaces fed_mat anc clears trhe cache. 
## in_matrix is the where the inverse of fed_matrix that gets cached. 

## Write a short comment describing this function

## cacheSolve() will either 1. solve the inverse if cacheSolve() hasn't been run 
## since creating the list or 2. return the inverse if it's been run before. 

## I haven't renamed x here, because x in this function is a list, just as it 
## was in the example. I guess I'm going half heartedly into "fussy" notation. 

cacheSolve <- function(x, ...) {
    in_matrix <- x$get.m.inverse()
        ## If: That cahce isn't empty, serve it up.
    if(!is.null(in_matrix)) {
        message("we did that already, getting cached data")
        return(in_matrix)
    }
        ## Else: FINE I'LL DO IT MYSELF.
    data <- x$get.m() 
    in_matrix <- solve(data, ...)
    x$set.m.inverse(in_matrix)
    in_matrix
        ## Return a matrix that is the inverse of 'x'
}
## I am still curious about the additional arguments that could be passed to 
## this function, but want to be done with this. 

## I suppose a point of somewhat obscure notation in the original (not "fussy"
## or pedantic as Alan Berger describes it in his forum tips) is the structure
## of these functions are generalizable. As we are learners, I think making 
## the notation more specific makes it more accessible to me. 