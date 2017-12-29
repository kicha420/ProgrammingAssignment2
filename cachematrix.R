## The first function (makeCacheMatrix) will create a cache list (special vector) 
## and the second function (cacheSolve) will give the inverse of the matrix. The 
## second function will check if the value is available in the cache, if yes then
## it takes the cache value else it computes and assigns the value to the cache.

## It creates a list of special vector to set, get, getinverse, setinverse values.

makeCacheMatrix <- function(x = matrix()) {
  mat_inv<-NULL
  set<-function(y){
        x <<- y
        mat_inv <<- NULL
  }
  get <- function() x
  setmat_inv <- function(mat_inverse) mat_inv <<- mat_inverse
  getmat_inv <- function() mat_inv

  list(set = set, get = get,setmat_inv = setmat_inv, getmat_inv = getmat_inv)
}


## it creates the inverse of the matrix.

cacheSolve <- function(x, ...) {

  mat_inv<-x$getmat_inv()

  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  my_mat<-x$get()
  mat_inv<-solve(my_mat,...)
          ## Return a matrix that is the inverse of 'x'
  x$setmat_inv(mat_inv)
  mat_inv
}
