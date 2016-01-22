

makecachematrix <- function(x = matrix()) {
        
        
        i = NULL
        set = function(y) {
                
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) i <<- inverse 
        getinverse = function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##Make cachematrix builds a list that allows a matrix and 
## the inverse of a matrix to be cached for future use
cachesolve <- function(x, ...) {
        ##Return a matrix that is the inverse of x
        
        i = x$getinverse()
        
        if (!is.null(i)){
                
                message("getting cached data")
                return(i)
        }
        
        data = x$get()
        i = solve(data)
        
        
        x$setinverse(i)
        
        return(i)
}

##returns the inverse matrix set my makecachematrix.  It will first try and find the cached inverse,
##or if the cached inverse does not exist, make a new matrix

##test

##tmatrix <- matrix(rnorm(100),10,10)

##tstcache <- makecachematrix(tmatrix)

##tstcache$get()

##cachesolve(tstcache)

##inverse if found by using solve
