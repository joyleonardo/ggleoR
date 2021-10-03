#' Title
#'
#' @param f  the function with any variables
#' @param x  the gussing start point
#' @param tol Accuracy ex(0.001)
#' @param N The maximum number of iterations
#'
#' @return the maximum point
#' @export
#'
#' @examples
ggleoNT=function(f,x,tol,N){
  diff<-4
  iter<-0
  while ((diff>tol) && (iter<N)) {
    oldx<-x
    x<-x-solve(numDeriv::hessian(ly, x))%*%numDeriv::grad(ly, x)
    diff=sqrt(sum((x-oldx)^2))
    iter<-iter+1
    cat('this is ',iter,'th iteration,', 'x is ',x,'the value is',ly(x), ',difference norm is',diff,'\n')
  }
  return(x)
}

devtools::document()
