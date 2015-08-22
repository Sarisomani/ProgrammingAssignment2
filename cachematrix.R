makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(),...) 
{    
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix) 
  x$setmatrix(m)
  m
}

A<-matrix(c(2,0,0,2),nrow=2,ncol=2)
ob1<-makeCacheMatrix(A)
ob2<-cacheSolve(ob1)
ob2

ob1$set(matrix(c(1,0,0,1),nrow=2,ncol=2))
ob2<-cacheSolve(ob1)
ob2