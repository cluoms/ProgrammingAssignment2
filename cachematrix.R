## The combination of makeCacheMatrix() and cacheSolve() allows you to store 
## the inverse of a matrix in the cache and conveniently retrieve, instead of
## re-calculate, the value later.

## makeCacheMatrix() takes a matrix as argument and stores its inverse in the
## cache.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        setinv<-function(y){
                inv<<-solve(y)
        }
        get<-function()x
        getinv<-function()inv
        list(setinv=setinv,get=get,getinv=getinv)
}


## The argument of cacheSolve() should be an object whose value is the result of
## makeCacheMatrix().
## cacheSolve() retrieve the matrix inverse if it has been stored in the cache,
## otherwise it calculates the result which will then be automatically recorded
## in the cache.

cacheSolve <- function(x) {
        inv=x$getinv()
        if(!is.null(inv)){
                message("Getting cached inverse")
                return(inv)
        }else{
                message("Calculating inverse")
                data<-x$get()
                x$setinv(data)
                return(x$getinv())        
        }
        
}
