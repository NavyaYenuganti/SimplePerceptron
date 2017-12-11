#' @title MY lineraly seperable data
#'
#' @description linearly seperable data to simple perceptron
#'
#' @param NULL
#'
#' @return NULL
#'
#' @examples
#'
#' @export

mydata<-function(){
x0<<-rep(1,100)
x1<<-runif(100,-10,10)
x2<<-runif(100,-30,30)
y0<<-rep(1,100)
y1<<-runif(100,-1,1)
y2<<-runif(100,-9,9)
d1<<-data.frame(x0,x1,x2)
d2<<-data.frame(y0,y1,y2)
}
