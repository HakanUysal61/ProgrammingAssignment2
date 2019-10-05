
## This function will cache the inverse of a matrix.


## Write a short comment describing this function

makeCacheMatrix <- function(x) 
  
  {
  
  m = NULL
  if (nrow(x)!=ncol(x)) return(m)
  if (lnn==0)
  {
    lnm[1]<-nrow(x)
    buffer(2,x,lnm)
    print(lnm[1:10])
    m<-solve(x)
    buffer(2,m,lni)
    lnn<-1
    print("ilk")
    print(lnm[2:1+nrow(x)+ncol(x)])
    nx<-lnm[2:1+nrow(x)+ncol(x)]
    dim(nx)<-c(nrow(x),ncol(x))
    print(nx)
    return(m)
  }
  j<-1
  for (i in 1:lnn)
  {
    if (nrow(x)==lnm[j]) # check if matrix size is not equal skip
    {
      
      nx<-lnm[j+1:j+nrow(x)+ncol(x)]
      print(nx)
      dim(nx)<-c(nrow(x),ncol(x))
      print(nx)
      
      if ( identical(x,nx))
      {
        print("using cache")
        m<-lni[j+1:j+nrow(x)+ncol(x)]
        dim(m)<-c(nrow(x),ncol(x))
        print(m)
      }
    }
      if (!is.null(m)) break # if cached matrix found.
    j<-j+lnm[j]+lnm[j]+1
  } # end of for loop to look cached matrixes.
  
  if (!is.null(m)) # a new matrix to cache.
    {
    lnm[j+1] <-nrow(x)
    buffer(j+1,x,lnm)
    m<-solve(x)
    buffer(j+1,m,lni)
    print("cached")
    lnn<- lnn+1
    
    }
  m
}


## will compute the inverse of a matrix returned by makeCacheMatrix function.


cacheSolve <- function(p,...) {
        ## Return a matrix that is the inverse of 'x'

        m<-makeCacheMatrix (p,...)
        m
  }

## Matrix to vector buffering
## start from position k, matrix x, to vector v

buffer <- function(k,mat,v)
{
  for (i in 1:nrow(mat))
       for (j in 1:ncol(mat))
         {
          mat[(i-1)*nrow(mat)+j+k-1]<-x[j,i]
       }
}

z<-c(1,2,3,4)
dim(z)<-c(2,2)
z2<-z
z3<-c(23,44,55,66,77,88,99,11,21)
dim(z3)<-c(3,3)
zw<-c(1,2,3,4,5,6)
dim(zw)<-c(3,2)
lnm<-rep(0,5000)
lni<-rep(0,5000)
lnn<-0
globalVariables(c("lmn","lmi","lnn"),add=TRUE)