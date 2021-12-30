#' bandwidth selection function
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param method the bandwidth method choice: CV or plug-in. Default is CV.
#'
#' @return Calculates the bandwidth value using cross validation or plug-in method (for localLin and localCst methods)
#' @import np
#' @import KernSmooth
#' @export
#'
#' @examples
#' #create a data frame
#' example<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the window value
#' bandwidth(x,y)
#'
bandwidth <- function(x, y, method="CV") {

  # Methode Cross-validation :
  if (method=="CV"){
    bw <- np::npregbw(formula = y~x)
    model <- np::npreg(bws = bw, gradients = TRUE) # Utile ? sinon -> bw$bw donne la bande passante directement
    result <- model$bw
  }

  # Methode Plug-in :
  else {
    if(method=="Plug-in"){
      hplug <- KernSmooth::dpill(x,y,gridsize=length(x))
      result <- hplug
    }

  # Choix manuel de la bande passante (il faut un reel positif) :
    else{
      if(as.numeric(method)>0){
        result = as.numeric(method)
      }

  # Message d'erreur si mauvais argument :
      else{
        message("The method selected is not supported by the function.")
      }
    }
  }
  return(result)
}




