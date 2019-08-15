
#' Permutation test for mutual information
#'
#' @importFrom infotheo mutinformation
#' @importFrom infotheo discretize
#' @importFrom plyr ddply
#'
#' @param X <object; input> A numeric vector.
#' @param Y <object; input> A numeric vector.
#' @param method <character; proccessing> a method for estimation of entropy.
#' @param disc.X <character; proccessing> a method for discretization of X.
#' @param disc.Y <character; proccessing> a method for discretization of Y.
#' @param n.sim <numeric; proccessing> The number of simulation.
#'
#' @export


MIPermute <- function(
  X,
  Y,
  method='emp',
  disc.X='equalfreq',
  disc.Y='equalfreq',
  n.sim = 2000
){

  D <- data.frame('X'=unname(X), 'Y'=unname(Y))

  df.pmt <- data.frame(
    i=seq(1:n.sim),
    j=seq(1:n.sim)
  )

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
  return(output)
}
