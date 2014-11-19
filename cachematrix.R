## This pair of functions creates a proxy for a numeric 
## matrix that will store the matrix and its proxy. The
## functions check to see if the inverse has already been 
## calculated.  If the inverse has - it returns that cached
## value, otherwise the inverse is calculated and stored.

## makeCacheMatrix creates the special list proxy of the 
## original numeric matrix. The function takes the original 
## numeric matrix as its argument and returns a special list
## which will reference both the original matrix and its inverse
##
## to create a proxy mat_proxy from a matrix my_matrix
## >mat_proxy<-makeCacheMatrix(my_matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function checks the special matix proxy created by 
## makeCacheMatrix if the inverse has already been calculated.
## If the inverse has been calculated, the inverse is returned.
## If the inverse has not been calculated the inverse is 
## returned and stored in the matrix proxy given as the 
## argument for cacheSolve.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
