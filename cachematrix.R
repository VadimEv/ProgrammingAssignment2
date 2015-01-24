## makes special "matrix" which is really a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## provides default if cachesolve is not yet executed
  set<-function(y){
    x<<- y
    m<<-NULL
  }
  ##sets value of the matrix
  get<-function() x
  setmatrix <- function(solve) m<<-solve
  getmatrix <- function() m
  ## to-do list: sets value of matrix, reads the value, set the value of inverse matrix and gets its value
  list(set=set,get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## solves cached matrix and either returns inverse matrix or displayes delay message and returns inverse matrix
## once finished

cacheSolve <- function(x=matrix(), ...) {
       m<-x$getmatrix()
       if(!is.null(m))
       {message("computating inverse matrix, please wait")
        return(m)
       }
       matrix<-x$get()
## now lets check if our matrix is invertible, i.e. matrix determinant is nonzero       
       if(det(matrix, ...) == 0)
       {
         message("matrix specified is not invertible")
         break
       }
       else
       {
       m<-solve(matrix, ...)
       x$setmatrix(m)
       }
       m
}
