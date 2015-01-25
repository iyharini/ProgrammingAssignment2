## First creating a matrix object that can cache its inverse.
## Then CachSolve retrieves the inverse using Solve()

## Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
minverse<-NULL #Setting value to Null and storing result
       set<-function(y){
             x<<-y
             minverse<<-NULL #Setting value to Null
         }
       get<-function() x
       setinverse<-function(solve) minverse<<- solve
       getinverse<-function() minverse #Returning the matrix inverse
       #Returning a list containing 4 functions to set matrix, get matrix, get matrix inverse, set matrix inverse
       list(set=set, get=get,
                       setinverse=setinverse,
                       getinverse=getinverse)
}


## Returns a marix which is the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         minverse<-x$getinverse() #Null if uncalculated and if not, returns the calculated matrix inverse
         if(!is.null(minverse)){
               message("getting cached data")
               return(minverse)
           }
        matrix<-x$get()
        minverse<-solve(matrix, ...)
        x$setinverse(minverse)
     minverse #Returning result
}
