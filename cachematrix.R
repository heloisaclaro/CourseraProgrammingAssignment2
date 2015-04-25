## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL ##Set the value of object m to Null
        set<-function(y){ ##set the value of the matrix
                x<<-y ##caches the matrix so that the next function (cacheSolve) may see if it has changed
                m<<-NULL
        }
        get<-function() x ## getting the function
        setmatrix<-function(solve) m<<- solve ##setting the matrix
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. 

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix() ## getting the matrix m
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

##Now test- Return a matrix that is the inverse of 'x'
x <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
x2 <- makeCacheMatrix(x)
cacheSolve(x2)

cacheSolve(x2)
