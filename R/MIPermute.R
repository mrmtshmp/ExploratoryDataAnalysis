
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
  S=NULL,
  method='emp',
  disc.X='equalfreq',
  disc.Y='equalfreq',
  disc.S = 'none',
  alpha = 0.6,
  n.sim = 2000,
  ...
){

  D <- data.frame('X'=unname(X), 'Y'=unname(Y))

  if(!is.null(S)) D$S <- unname(S)

  print(try(ftable(D$S,D$X,D$Y)))


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

        if(is.null(S)){
          m1a <- infotheo::mutinformation(
            X=discretize(D$X, disc = disc.X, nbins = ),
            Y=discretize(D$Y, disc = disc.Y, nbins = ),
            method = method
            )
          }
        if(!is.null(S)){
          m1a <- infotheo::condinformation(
            X=discretize(D$X, disc = disc.X, nbins = ),
            Y=discretize(D$Y, disc = disc.Y, nbins = ),
            S=discretize(D$S, disc = disc.S, nbins = ),
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
              }
            if(!is.null(S)){
              if(disc.S=='none'){
                m1a <- infotheo::condinformation(
                  X=as.factor(D$X),
                  Y=as.factor(D$Y),
                  S=as.factor(D$S),
                  method = method
                  )
              }else{
                m1a <- infotheo::condinformation(
                  X=as.factor(D$X),
                  Y=as.factor(D$Y),
                  S=discretize(D$S, disc = disc.S, nbins = ),
                  method = method
                  )
                }
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
