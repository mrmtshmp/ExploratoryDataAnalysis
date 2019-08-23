
#' Permutation test for mutual information
#'
#' @import minerva
#'
#' @importFrom infotheo mutinformation
#' @importFrom infotheo discretize
#' @importFrom plyr ddply
#'
#' @param X <object; input> A numeric vector.
#' @param Y <object; input> A numeric vector.
#' @param method <character; proccessing> a method for estimation of entropy. (see help of infotheo- or minerva-package)
#' @param disc.X <character; proccessing> a method for discretization of X.
#' @param disc.Y <character; proccessing> a method for discretization of Y.
#' @param mine.alpha <numeric; proccessing> alpha parameter for the mine method. (works only when *method* is "mine")
#' @param n.sim <numeric; proccessing> The number of simulation.
#' @param ... Parameters to be passed to the function *minerva::mine*.
#'
#' @export


MIPermute <- function(
  X,
  Y,
  method='emp',
  disc.X='equalfreq',
  disc.Y='equalfreq',
  alpha = 0.6,
  n.sim = 2000,
  ...
){

  D <- data.frame('X'=unname(X), 'Y'=unname(Y))

  df.pmt <- data.frame(
    i=seq(1:n.sim),
    j=seq(1:n.sim)
  )

  if(
    method !="MIC" &
    disc.X != "none" &
    disc.Y != "none"
    ){
    output <- ddply(
      df.pmt,
      .(i),
      function(itt){
        i.numb <- unique(itt$i)
        if(i.numb > 1) D$Y <- sample(D$Y)
        m1a <- infotheo::mutinformation(
          X=discretize(D$X, disc = disc.X, nbins = ),
          Y=discretize(D$Y, disc = disc.Y, nbins = ),
          method = method
          )
        return(m1a)
        }
      )
    }else{
      if(
        method !="MIC" &
        disc.X == "none" &
        disc.Y == "none"
        ){
        output <- ddply(
          df.pmt,
          .(i),
          function(itt){
            i.numb <- unique(itt$i)
            if(i.numb > 1) D$Y <- sample(D$Y)
            m1a <- infotheo::mutinformation(
              X=D$X,
              Y=D$Y,
              method = method
              )
            return(m1a)
            }
          )
        }else{
          output <- ddply(
            df.pmt,
            .(i),
            function(itt){
              i.numb <- unique(itt$i)
              if(i.numb > 1) D$Y <- sample(D$Y)
              m1a <- minerva::mine(
                x=(D$X),
                y=(D$Y),
                alpha = alpha,
                ...
              )
              return(c(m1a$MIC, m1a$MAS, m1a$MEV, m1a$MCN, m1a$`MIC-R2`, m1a$GMIC, m1a$TIC))
              }
            )
          }
  return(output)
}
