#' Cubic Penalized B-splines quantile regression
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param d differentiation order - 1, 2 or 3. Default is set to d=3.
#' @param cents numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97)).
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#' @param axes.lab NULL or c("Nom_axe_X, Nom_axe_Y").
#'
#' @import graphics
#' @import quantregGrowth
#' @export
#' @return Plots the curves at centiles selected and returns an object of class gcrq.
#'
#' @examples
#' #create a sample data frame
#' weights=c(500,600,1000,1150,1200,1260,1240,1300,1370,1500,2000,2100,2150,2500,
#' 2800,2900,3050,3200,2980,3000,3300,3100,3200,3600,3500,3700,3900,3900,4000,
#' 4200,3000,4500,4300,4900,4350,3700,4000,5000,4300)
#' age<-c(30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,
#' 37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42)
#' sample<-data.frame(age,weights)
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`

#' bsplines(x,y,lambdas=seq(1,100))

bsplines <- function(x, y, lambdas, d=3, cents=c(0.03,0.25,0.5,0.75,0.97), leg=TRUE, axes.lab =NULL){

  colors <- c("green","blue","red","blue","green") # Couleur des courbes
  lty = c(2,2,3,2,2) # Epaisseur des courbes

  ymin = min(y)
  ymax = max(y)

  # Initialisation du graph :
  xlab=""
  ylab=""
  if (is.null(axes.lab) | typeof(axes.lab) != "character" | length(axes.lab) != 2){
    plot(x, y, lwd=1, xlab =xlab, ylab=ylab, ylim=c(ymin,ymax))
  }else{
    xlab = axes.lab[1]
    ylab = axes.lab[2]
    plot(x, y, lwd=1, xlab =xlab, ylab =ylab, ylim=c(ymin,ymax))
  }
  graphics::par(new=TRUE)

  # Calcul de la courbe

  fit <- quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=d), tau=cents)

  plot(fit, col=colors, lwd=2, lty=lty, main=paste("Cubic B-Splines : d =",toString(d)), xlab='',ylab='', ylim=c(ymin,ymax), axes=FALSE)


  if (leg){
  # Creation d'un vecteur centiles, ou ils sont en chaine de caracteres (pour la legende) :
  lege = c()
  for (i in cents){lege = c(lege, toString(i))}

  # Affichage de la legende
  graphics::legend('bottomright',
         legend = lege,
         col = colors,
         lty = lty,
         title = paste("lambda : ", toString(fit$info.smooth$lambda))
  )
  }
  return(fit)
}

