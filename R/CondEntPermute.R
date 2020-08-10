
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


CondEntPermute <- function(
  X,
  Y1,
  Y2,
  method='emp',
  disc.X='equalfreq',
  disc.Y1='equalfreq',
  disc.Y2='equalfreq',
  alpha = 0.6,
  n.sim = 2000,
  ...
){

  D <- data.frame('X'=unname(X), 'Y1'=unname(Y1), 'Y2'=unname(Y2))

  df.pmt <- data.frame(
    i=seq(1:n.sim),
    j=seq(1:n.sim)
  )

  if(
    disc.X != "none" &
    disc.Y1 != "none" &
    disc.Y2 != "none"
    ){
    output <- ddply(
      df.pmt,
      .(i),
      function(itt){
        i.numb <- unique(itt$i)
        if(i.numb > 1) {
          D.randsam <- D %>%
            gather(var, val, -X)
          D$Y <- sample(D$Y)
          }

        if(is.null(S)){
          m1a <- infotheo::mutinformation(
            X=discretize(D$X, disc = disc.X, nbins = ),
            Y=discretize(D$Y, disc = disc.Y, nbins = ),
            S=D$S,
            method = method
            )
        }else{
          m1a <- infotheo::condinformation(
            X=discretize(D$X, disc = disc.X, nbins = ),
            Y=discretize(D$Y, disc = disc.Y, nbins = ),
            S=D$S,
            method = method
          )
          }
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
            if(is.null(S)){
              m1a <- infotheo::mutinformation(
                X=as.factor(D$X),
                Y=as.factor(D$Y),
                method = method
                )
            } else{
              m1a <- infotheo::condinformation(
                X=as.factor(D$X),
                Y=as.factor(D$Y),
                S=D$S,
                method = method
                )
              }
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
              df.m1a <- data.frame(m1a$MIC, m1a$MAS, m1a$MEV, m1a$MCN, m1a$`MIC-R2`, m1a$GMIC, m1a$TIC)
              colnames(df.m1a) <- c('MIC', 'MAS', 'MEV', 'MCN', 'MIC-R2', 'GMIC', 'TIC')
              return(df.m1a)
              }
            )
        }
      }
  return(output)
}
