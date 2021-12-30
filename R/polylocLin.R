#' Polynomial local linear estimator
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97)).
#' @param disp_window Boolean. Should the scale of bandwidth be displayed (TRUE) or not (FALSE).
#' @param leg Boolean. Should the legend be desplayed (TRUE) or not (FALSE).
#' @param axes.lab NULL or c("Nom_axe_X, Nom_axe_Y").
#'
#' @return Plots the centile curves and returns a list object containing bandwidth value and estimated centiles values.
#' @import graphics
#' @import quantreg
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
#' #calculate the centile and plot the curves
#' polylocLin(x,y)

polylocLin <- function(x, y, bandwidth.method="CV", cents=c(0.03,0.25,0.5,0.75,0.97), disp_window = TRUE, leg = TRUE, axes.lab=NULL){

  # Calcul de la fenetre/bande passante :
  bandwidth <- bandwidth(x, y, method=bandwidth.method)


  # Tracage du nuage d'observations
  xlab=""
  ylab=""
  if (is.null(axes.lab) | typeof(axes.lab) != "character" | length(axes.lab) != 2){
    plot(x, y, lwd=1, xlab =xlab, ylab=ylab )
  }else{
    xlab = axes.lab[1]
    ylab = axes.lab[2]
    plot(x, y, lwd=1, xlab =xlab, ylab =ylab)
  }
  graphics::title(main='Local linear estimator')

  # Parametres courbes :
  colors <- c("green","blue","red","blue","green") #couleur des courbes
  lty=c(2,2,3,2,2) # epaisseur des courbes


  # Regression et plot :
  for (i in 1:length(cents)){
    fit <- quantreg::lprq(x, y,h=bandwidth, tau=cents[i], m=500)
    lines(fit$xx,fit$fv, col=colors[i],lty=lty[i],lwd=2)
  }


  # Affichage d'un trait representant la bande passante (en haut a gauche) :
  if(disp_window){
    usr = graphics::par('usr') # donne les coord des extremes du plot

    x1 = usr[1]
    x2 = usr[2]
    y1 = usr[3]
    y2 = usr[4]

    Y = y2 - 0.05*(y2-y1)
    X1 = x1 + 0.05*(x2-x1)

    X2 = X1 + bandwidth
    graphics::segments(X1, Y, X2, Y, lwd = 3)


    Y1 = Y + 0.5*(y2-Y)
    Y2 = Y - 0.5*(y2-Y)
    graphics::segments(X1, Y1, X1, Y2, lwd =3)
    graphics::segments(X2, Y1, X2, Y2, lwd =3)
  }

  # Affichage de la legende
  if (leg){
    lege = c()
    for (i in cents){lege = c(lege, toString(i))}

    # Affichage de la legende
    graphics::legend('bottomright',
                     legend = c(paste("bandwidth : ", toString(round(bandwidth, 3))),
                                lege),
                     col = c("black", colors),
                     lty = c(1,lty)
    )
  }
}
