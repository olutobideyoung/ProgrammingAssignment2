## This function creates a special "matrix" object 
##that can cache its inverse


## Create object "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()){
  inv<-inv<-NULL
  set<-function(y){
    inv <<- NULL
  }
  get<-function()(x)
  setInverse <- function(inverse)(inv<<-inverse)
  getInverse <- function()(inv)
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## Solving the inverse of the special matrix
cacheSolve <- function(x,...){
 inv <<-x$getInverse()
 if(!is.null(inv)){
   message("getting cached data")
   return(inv)
 }
 mat <<-x$get()
 inv <- solve(mat,...)
 x$getInverse(inv)
 inv
}
        ## Return a matrix that is the inverse of 'x'

my_matrix <-makeCacheMatrix(matrix(1:16, nrow=4, ncol=4))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)

