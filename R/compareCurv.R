#' Centile curves using local polynomial compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwidth. Could be cross validation or Plug-in. Default is set to CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#' @param data the noise data we want to compare
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#' @return Plots centile curves with local constant polynomial and displays them on the same figure as the noise data to be compared
#'
#' @import locpol
#' @import graphics
#' @import stats
#'
#' @export
#' @examples
#' #create an example data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' locNormCstData(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.50,0.75,0.97),data=abnormal)
#'
#'
locNormCstData<-function(x, y, bandwidth.method, cents=c(0.03,0.25,0.5,0.75,0.97), data, leg=FALSE){
  locNormCst(x, y, bandwidth.method=bandwidth.method, kernel = locpol::gaussK, cents=cents, disp_window = FALSE, leg = leg)
  graphics::par(new=T)
  plot(data, type= 'p', col="orange", pch='*', cex=2, axes=FALSE, xlab='', ylab='')

}
#' Centile curves using local linear polynomial compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwidth. Could be cross validation or Plug-in. Default is set to CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97))
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#' @param data the noise data we want to compare
#' @return Plots centile curves with local linear polynomial using a Gaussian kernel and displays them on the same figure as the noise data to be compared
#'
#' @import locpol
#' @import graphics
#' @export
#'
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' locNormLinData(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.50,0.75,0.97),data=abnormal)
#'
#'
locNormLinData <- function(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data, leg=FALSE){
  locNormLin(x,y,bandwidth.method=bandwidth.method, kernel = locpol::gaussK, cents=cents, disp_window = FALSE, leg = leg)
  graphics::par(new=T)
  plot(data, type= 'p', col="orange", pch='*', cex=2, axes=FALSE, xlab='', ylab='')
}

#' Polynomial local linear estimator compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97)).
#' @param data the noise data we want to compare
#' @param leg Boolean. Should the legend be displayed (TRUE) or not (FALSE).
#'
#' @return Plots the centile curves and data points to compare with.
#' @import graphics
#' @import quantreg
#' @export
#'

#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' polylocLinData(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.50,0.75,0.97),data=abnormal)
#'
#'
polylocLinData<-function(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.5,0.75,0.97), data, leg=FALSE){
  polylocLin(x, y, bandwidth.method=bandwidth.method, cents=cents, disp_window = FALSE, leg = leg, axes.lab=NULL)
  graphics::par(new=T)
  plot(data, type= 'p', col="orange", pch='*', cex=2, axes=FALSE, xlab='', ylab='')
  }

#' Centile curves using B-splines compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param data the noise data to be compared to
#' @param lambdas to be set by user. Can be a vector or a single numeric value. Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97))
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#'
#' @return Plots centile curves with B-splines of different differential orders (d) and displays them on the same figure as the noise data
#' @export
#'
#' @import quantregGrowth
#' @import graphics
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,30,rep=TRUE),sample(800:5000,30,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,6,rep=TRUE),sample(800:5000,6,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' bSplinesData(x,y,lambdas=1,abnormal)
#'
#'
bSplinesData<-function(x,y,lambdas,data,cents=c(0.03,0.25,0.5,0.75,0.97), leg=FALSE){
  bsplines(x, y, lambdas=lambdas, d=3, cents=cents, leg = leg)
  graphics::par(new=T)
  plot(data, type= 'p', col="orange", pch='*', cex=2, axes=FALSE, xlab='', ylab='')
}
#' Centile curves according to different methods
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwdth. Could be cross validation or Plug-in. Default is set to CV.
#' @param lambdas to be set for "B-Splines". Can be a vector or a single numeric value. Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param data the abnormal/external data we want to compare the curves with
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#' @return Plots centile curves with the different methods and displays them on the same figure as the noise data to compare
#' @export
#'
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,30,rep=TRUE),sample(800:5000,30,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,6,rep=TRUE),sample(800:5000,6,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' compareCurv(x,y,bandwidth.method="Plug-in",lambdas=1,abnormal)

compareCurv<-function(x,y,bandwidth.method="CV",lambdas,data,leg=FALSE){
  #message('Make sure to load locpol and quantregGrowth using library()')
  graphics::plot.new()
  graphics::par(mfrow=c(2,2))
  #1
  locNormCstData(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data=data,leg=leg)
  #2
  locNormLinData(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data=data,leg=leg)
  #3
  polylocLinData(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data=data,leg=leg)
  #4
  bSplinesData(x,y,lambdas=lambdas,data=data,cents=c(0.03,0.25,0.5,0.75,0.97), leg=leg)
}

