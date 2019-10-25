#' Simply do LOCF.
#'
#' @param paper <object; input> A data frame
#' @param ink <object; input> A data frame
#' @param mold <object; input> A data frame
#' @param val.Visit <object; proccessing> A numeric vector
#'
#' @export

mf.LOCFbyWideData <-
  function(
    paper = ADS.measurement,
    ink   = ADS.measurement,
    mold  = df.miss.measurement,
    val.Visit
  ){
    DUMMY <- rep(0, nrow(ink))
    names(DUMMY) <- "Dummy"
    ink   <-
      cbind(
        DUMMY,
        ink[
          ,
          sprintf("%s.%s", "vis", val.Visit[1:(length(val.Visit)-1)])
          ]
      )
    mold  <-
      mold[
        ,
        sprintf("%s.%s", "miss", val.Visit)
        ]
    inked_mold <- mold*ink
    colnames(inked_mold) <- sprintf("%s.%s", "vis", val.Visit)

    inked_mold[
      !is.na(
        paper[,sprintf("%s.%s", "vis", val.Visit)]
      ) &
        is.na(inked_mold)
      ] <- 0

    paper[is.na(paper)] <- 0
    paper[,sprintf("%s.%s", "vis", val.Visit)] <-
      paper[,sprintf("%s.%s", "vis", val.Visit)] + inked_mold
    return(paper)
  }
