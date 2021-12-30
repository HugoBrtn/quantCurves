#' Centile curves based on each of the  four different methods
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV (for Local Linear and Local Constant estimators)
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector (for penalized B-spline estimator).
#'
#' @return Four graphs, one for each of the following methods : Local Linear, Local Constant, Cubic Splines and penalized B-splines.
#' @export
#'
#' @examples
#' #create a sample data frame
#' weights=c(500,600,1000,1150,1200,1260,1240,1300,1370,1500,2000,2100,2150,2500,
#' 2800,2900,3050,3200,2980,3000,3300,3100,3200,3600,3500,3700,3900,3900,4000,
#' 4200,3000,4500,4300,4900,4350,3700,4000,5000,4300)
#' age<-c(30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,
#' 37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42)
#' sample<-data.frame(weights,age)
#' colnames(sample)<-c("Weight in gramms","Gestational Age in weeks")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' fourCurv(x,y, lambdas=seq(1,10))
#'
fourCurv<-function(x, y, bandwidth.method="CV", lambdas){
  graphics::plot.new()
  graphics::par(mfrow=c(2,2))
  locNormCst(x, y, bandwidth.method, cents=c(0.03,0.25,0.5,0.75,0.97), leg =FALSE)
  locNormLin(x, y, bandwidth.method, cents=c(0.03,0.25,0.5,0.75,0.97),leg=FALSE)
  polylocLin(x, y, bandwidth.method, cents=c(0.03,0.25,0.5,0.75,0.97), leg = FALSE)
  bsplines(x, y, lambdas=lambdas, cents=c(0.03,0.25,0.5,0.75,0.97),leg=FALSE)
}
