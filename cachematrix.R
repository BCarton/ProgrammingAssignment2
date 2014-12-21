## Duo of functions 'makeCacheMatrix()' and 'cacheSolve()' that wrap R's matrix functionality
## with a caching facililty.

## Usage:
##   Given an R language matrix object A, call...
##     M <- makeCacheMatrix(A)
##     A_inverse <- cacheSolve(M)
##   Use M$get() to access the wrapped standard R language matrix
##
## History:
##             Based (heavily) on the sample makeVector()
## 2014-12-19: Test function initially created. Write the tests first!
## 2014-12-20: Changed attribute names to A, A_inv.
##             (Upper case because it looks better for matrixes - to me anyway. More 'mathematical')
## 2014-12-21: Added counter 'cache_hits' to confirm cache usage

## makeCacheMatrix
##   This creates a 'CacheMatrix' - which is actually a list with
##   a matrix and (later) its cached inverse as elements
##
makeCacheMatrix <- function(A = matrix()) {
    A_inv <- NULL
    cache_hits <- 0
    set <- function(A_prime) {
        A <<- A_prime
        A_inv <<- NULL
        cache_hits <<- 0
    }
    get <- function() A
    set_inverse <- function(inv) A_inv <<- inv
    get_inverse <- function() A_inv
    incr_cache_hits <- function() cache_hits <<- cache_hits+1
    get_cache_hits <- function() cache_hits
    list(set = set, get = get,
         set_inverse = set_inverse, get_inverse = get_inverse,
         incr_cache_hits = incr_cache_hits, get_cache_hits = get_cache_hits)
}

## cacheSolve
##   This calls solve() on a 'CacheMatrix' the first time to return
##   its inverse, then accesses the cached version of this thereafter
##
cacheSolve <- function(M, ...) {
    A_inv <- M$get_inverse()
    if(!is.null(A_inv)) {
        #message("getting cached data")
        M$incr_cache_hits()
    } else {
        A_inv <- solve(M$get(), ...)
        M$set_inverse(A_inv)
    }
    A_inv
}

##
## Tests to pass
##
testCacheMatrix <- function(runtests = 1:3) {
  if (1 %in% runtests) {
    # Test that matrix %*% inverse is the unit matrix
    test = NULL
    theta <- pi/6
    A <- matrix(rbind(
        c(cos(theta), sin(theta)),
        c(-sin(theta), cos(theta))
      ), 2, 2
    )
    M <- makeCacheMatrix(A)
    B <- cacheSolve(M)
    if (sum(abs(A %*% B - diag(2))) < 1e-12) {
      test <- "PASS"
    } else {
      test <- "FAIL"
    }
    print(sprintf("Test 1: %s", test))
  }
  if (2 %in% runtests) {
    # Test that the cache is being accessed as we expect
    test = NULL
    theta <- pi/6
    A <- matrix(rbind(
      c(cos(theta), sin(theta)),
      c(-sin(theta), cos(theta))
    ), 2, 2
    )
    M <- makeCacheMatrix(A)
    cacheSolve(M)
    if (M$get_cache_hits() == 0) {
      cacheSolve(M)
      cacheSolve(M)
      cacheSolve(M)
      if (M$get_cache_hits() == 3) {
        test <- "PASS"
      } else {
        test <- "FAIL"
      }
    } else {
      test <- "FAIL"
    }
    print(sprintf("Test 2: %s", test))
  }
  if (3 %in% runtests) {
    # Test instances don't run over each other's data
    test = NULL
    theta <- pi/6
    A1 <- matrix(rbind(
      c(cos(theta), sin(theta)),
      c(-sin(theta), cos(theta))
    ), 2, 2
    )
    M1 <- makeCacheMatrix(A1)
    theta <- pi/4
    A2 <- matrix(rbind(
      c(cos(theta), sin(theta)),
      c(-sin(theta), cos(theta))
    ), 2, 2
    )
    M2 <- makeCacheMatrix(A2)
    cacheSolve(M1)
    cacheSolve(M2)
    cacheSolve(M1)
    cacheSolve(M2)
    B1 <- cacheSolve(M1)
    B2 <- cacheSolve(M2)
    theta <- pi/6
    A1 <- matrix(rbind(
      c(cos(theta), sin(theta)),
      c(-sin(theta), cos(theta))
    ), 2, 2
    )
    if ((sum(abs(A1 %*% B1 - diag(2))) < 1e-12) && (sum(abs(A2 %*% B2 - diag(2))) < 1e-12)) {
      test <- "PASS"
    } else {
      test <- "FAIL"
    }
    print(sprintf("Test 3: %s", test))
  }
}
#testCacheMatrix(c(1,2,3))

