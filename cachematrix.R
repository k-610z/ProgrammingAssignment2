## makeChacheMatrix stores a list 
## This list can:
## create matrix(set), return matrix(get)
##compute inverse of matrix(setmatrix) and return inverse matrix(getmatrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve will compute inverse but it checks if it is calculated already 
## if calculated already, 
##cacheSolve returns the calculated matrix (caches the matrix)

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
