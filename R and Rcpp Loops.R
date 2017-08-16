## use aaply -- the simplest code
require(plyr)
testfun0 <- function(x, y)  aaply(x, 1, function(x) rbinom(1, x, y)
                                  
## rewrite the above as an explicit loop
testfun1 = function(nrep, x, y) {
  ## loop index
  ii<-0;
  ## result vector
  ret<-rep(0, nrep);
  while(ii < nrep) {
    ii<-ii+1;
    ## how many successes for each element of bb?
    ret[ii] <- rbinom(1, x[ii], y)
  }
  return(ret)
}

## define source code as string, pass to inline
src <-  ' 
IntegerVector tmp(clone(x));
double rate = as< double >(y); 

int tmpsize = tmp.size(); 

RNGScope scope; 

for (int ii =0; ii < tmpsize; ii++) {
tmp(ii) = Rf_rbinom(tmp(ii), rate);
};

return tmp;
'

require(inline)
## compile the function, inspect the process with verbose=T
testfun2 = cxxfunction(signature(x='integer', y='numeric'), src, plugin='Rcpp', verbose=T)

## timing function
ps <- function(x) print(system.time(x))

## Input vector
bb <- rbinom(1e5, 20, 0.5)
## test each case for 2 different loop lengths 
for( nrep in c(1e4, 1e5)){
  ## initialize RNG each time with the same seed
  ## plyr
  set.seed(20); ps(cc0<- testfun0(nrep[1:nrep], bb, 0.1))
  ## explicit while loop
  set.seed(20); ps(cc1<- testfun1(nrep, bb, 0.1))
  ## Rcpp
  set.seed(20); ps(cc2<- testfun2(bb[1:nrep], 0.1))
  print(all.equal(cc1, cc2))
}

