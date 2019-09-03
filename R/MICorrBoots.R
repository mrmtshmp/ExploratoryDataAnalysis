#' Mutual information with weighted bootstrap
#'
#' @import minerva
#'
#' @importFrom infotheo mutinformation
#' @importFrom infotheo condinformation
#' @importFrom infotheo discretize
#' @importFrom plyr ddply
#'
#' @param X <object; input> A numeric vector.
#' @param Y <object; input> A numeric vector.
#' @param method <character; proccessing> a method for estimation of entropy. (see help of infotheo- or minerva-package)
#' @param disc.X <character; proccessing> a method for discretization of X.
#' @param disc.Y <character; proccessing> a method for discretization of Y.
#' @param alpha <numeric; proccessing> alpha parameter for the mine method. (works only when *method* is "mine")
#' @param n.sim <numeric; proccessing> The number of simulation.
#' @param group <vector; proccessing> Ve
#' @param samp.prob <numeric vector; proccessing> probability
#' @param ... Parameters to be passed to the function *minerva::mine*.
#'
#' @examples
#' eg.res.MICorrBoots <- MICorrBoots(
#' X = c( 1,  1,  2,  3,  5,  2,  2,  3,  1),
#' Y = c( 1,  1,  2,  3,  5,  1,  1,  1,  2),
#' S = c('A','A','A','A','A','B','B','B','B'),
#' method='shrink',
#' disc.X='none',
#' disc.Y='none',
#' alpha = NULL,
#' n.sim = 50,
#' group = c('A','A','A','A','A','B','B','B','B'),
#' samp.prob =
#'   rep(
#'    1/length( unique( c('A','A','A','A','A','B','B','B','B'))),
#'    length( unique( c('A','A','A','A','A','B','B','B','B')))
#'    )
#'   )
#'
#'
#' @export

MICorrBoots <- function(
  X,
  Y,
  S=NULL,
  method='emp',
  disc.X='equalfreq',
  disc.Y='equalfreq',
  alpha = 0.6,
  n.sim = 2000,
  group = ADS$subjID,
  samp.prob =
    rep(
      1/length(unique(ADS$subjID)),
      length(unique(ADS$subjID))
    ),
  ...
){

  D <- data.frame('X'=unname(X), 'Y'=unname(Y), 'S'=unname(S))

  df.pmt <- data.frame(
    i=seq(1:n.sim),
    j=seq(1:n.sim)
  )


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

        if(i.numb > 1){
          D.boot <- data.frame()
          D.boot <- ddply(
            D,
            .(S),
            function(d){
              unique.subj.D <- unique(D$S)
              D.boot <-
                D[
                  D$S==
                    unique.subj.D[
                      sample(
                        size = 1,
                        x = 1:length(unique.subj.D),
                        prob = samp.prob,
                        replace=TRUE
                      )
                      ],
                  ]
              D.boot$ori.ID <- unique(d$S)
              return(D.boot)
            }
          )
        } else {
          D.boot <- D
          D.boot$ori.ID <- D$S
        }


        X=factor(D.boot$X, levels=unique(c(D.boot$X, D.boot$Y)))
        Y=factor(D.boot$Y, levels=unique(c(D.boot$X, D.boot$Y)))
        print(sprintf("%s_%s,%s",length(X),length(Y),length(as.factor(D.boot$ori.ID))))
        S=as.factor(D.boot$ori.ID)
        m1a_1 <-
          infotheo::condinformation(
            X=X,
            Y=Y,
            #              S=S,
            method = method
          )
        print("?")

        print(unique(c(X,Y)))
        print(X)
        print(Y)

        m1a_2 <-
          infotheo::condentropy(
            unique(X, Y),
            method=method
          )

        print("??")
        print(sprintf("condinfo_%s",length(m1a_1)))
        print(sprintf("condentr_%s",length(m1a_2)))

        m1a <- m1a_1/ m1a_2

        vec.consis <-
          data.frame(X=X,Y=Y) %>%
          mutate(
            consis= ifelse(X==Y, 1, 0)
          )

        consis <- mean(vec.consis$consis)

        return(m1a*consis)
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
