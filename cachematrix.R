
makeCacheMatrix <- function(x = matrix()) {
                # Create cached matrix object
                i <- NULL
                
                set <- function(y){
                        x<<- y
                        i<<- NULL
                }
                
                get <- function() x
                setInverse <- function(inv) i <<-inv
                getInverse <- function() i
                list (set = set, get=get, 
                      setInverse=setInverse, 
                      getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #ensure x got the right args and class
        arg_list <- c('get','set','setInverse','getInverse')
        stopifnot(class(x)=='list')
        stopifnot(sum(sapply(X=arg_list,FUN=function(X) sum(ls(x)==X)))==4)
        
        #check cache for inverse
        i <- x$getInverse()
        if(!is.null(i)){
                message('Getting Cached data')
                return(i)
        }
        data <-x$get()
        i <- solve(data,...)
        x$setInverse(i)

        
        
}
