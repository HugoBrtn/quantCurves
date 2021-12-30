#' Centile curves based on one of the different methods
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.select the bandwidth method choice: CV or plug-in. Default is CV.
#' @param lambdas set to 0. To be set if method chosen is "B-Splines". Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param method str - The method choosen for displaying the curve. Could be: "Local normal constant", "Local normal linear", "Polynomial local" or "B-splines".
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#' @param disp_window Boolean. Should the scale of bandwidth be displayed (TRUE) or not (FALSE).
#' @param d differentiation order - 1, 2 or 3. Default is set to d=3.
#' @param kernel the Kernel function that will be used in the algorithm ("trig", "gauss", "circ", "cubic" or "epan").
#'
#' @return Plots centile curves according to the chosen method
#' @import graphics
#' @export
#'

#' @examples
#' #create an example data frame
#'weights=c(500,600,1000,1150,1200,1260,1240,1300,1370,1500,2000,2100,2150,2500,
#'2800,2900,3050,3200,2980,3000,3300,3100,3200,3600,3500,3700,3900,3900,4000,
#'4200,3000,4500,4300,4900,4350,3700,4000,5000,4300)
#'age<-c(30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,
#'37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42)
#'sample<-data.frame(age,weights)
#'colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#'x<-sample$`Gestational Age in weeks`
#'y<-sample$`Weight in gramms`
#' CentCurv(x,y,method='Polynomial local')
#' CentCurv(x,y,method='B-Splines',lambdas=1)
#'
CentCurv<-function(x,y,bandwidth.select="CV",method,lambdas=0, kernel = locpol::gaussK, d=3, cents=c(0.03,0.25,0.5,0.75,0.97), disp_window = FALSE){
  if (method=="Local normal constant"){locNormCst(x,y,bandwidth.select, kernel=kernel, cents=cents, disp_window = disp_window)}
  else{if (method=="Local normal linear"){locNormLin(x,y, bandwidth.select, kernel=kernel, cents=cents, disp_window=disp_window)}
    else{if(method=="Polynomial local"){polylocLin(x, y, bandwidth.method=bandwidth.select, cents=cents)}
      else{if(method=="B-Splines"){bsplines(x,y,lambdas=lambdas,cents=cents, d=d)}
        else{stop("The method selected is not supported by the function")}}}}
}
