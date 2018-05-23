## makeCacheMatrix stores the inverse of a matrix as an object so that the cacheSolve function 
## could retrieve the value as needed.

## makeCacheMatrix is a function to create a list of functions that could:
## 1.set the value of the matrix; 2.get the value of the matrix; 
## 3.set the value of the inverse of the matrix 4.get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)
    x<<-y
  inv<<-NULL
  
  get<-function()x
  set_inv<-function(inverse)inv<<-inverse
  get_inv<-function()inv
  
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
  
}


## The cacheSolve function retrieves the inverse of a matrix from makeCacheMatrix
## If the inverse of the matrix is already stored, it then skips the computation
## Otherwise, it resets the value of the matrix and computates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv<-x$get_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data)
  x$set_inv(inv)
  inv
  
}
