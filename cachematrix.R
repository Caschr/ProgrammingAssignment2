## The functions below is designed to minimize time spent computing the inverse large matrices.
## The makeCacheMatrix will make a "special" matrix which will actually be a list that contains both the matrix and the inverse (if calculated). The function cacheSolve will do the actual computation,
## or if the inverse already exists, retrieve it from the matrix created witht the makeCacheMatrix function.

## This function will create a list with four elements, containing four functions. Each function will help to create the matrix descriped above.

makeCacheMatrix <- function(x = matrix()) {
                InverseMatrix <- NULL           ## Setting the inverseMatrix to NULL ensures the if test done in the cacheSolve will behave correctly

        set <- function(NewMatrix) {
                x <<- NewMatrix                 ## Sets the new matrix (given from the set function) to be the new matrix
                InverseMatrix <<- NULL          ## As a new matrix is given, any old inverse matrices must be deleted, by setting the InverseMatrix to NULL.
        }                                       ## Here it is important to use <<- since the inverse and the new matrix will be called from outside this environment.

        get <- function() x
        setInverse <- function(InverseMatrix_P) InverseMatrix <<- InverseMatrix_P        ## This function allows the cacheSolve to set the InverseMatrix to it has calculated. The "_P" represents the matrix
                                                                                        ## that was passed from the cacheSolve.
        getInverse <- function() InverseMatrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    ## Create the list that is returned.


}


## This function will do the actual computation of the inverse matrix. First it will test to see if the inverse matrix already exists, and if it does retrieve it.
## If it does not exists it will compute it and return it to the matrix (which is actually a list).

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()                         ## Retrieve the inverse from the matrix. If the inverse matrix does not exist, it will retrieve NULL (as we put in the above)
                if(!is.null(inverseMatrix)) {                   ## If is.null returns false, then the inverse has been computed, as because of the exclamation mark (!) it will be negated to TRUE,
                        message("Getting the inverse matrix")   ## and the if-clause will be run. In addition, the inverse matrix is returned and the program quits.
                        return(inverseMatrix)
                }
        Matrix_P <- x$get()                                     ## This retrieves the actual matrix from the passed matrix (in the function call)
        InverseMatrix <- solve(Matrix_P)                        ## This calculates the inverse matrix from the passed matrix
        x$setInverse(InverseMatrix)                                ## When the inverse has been calculated, we "save" it to the (the list containing four functions)
        InverseMatrix                                           ## Lastly we print the inverse matrix
}
