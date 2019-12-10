#' Interpolation.
#'
#' @param paper <object; input> A data frame.
#' @param ink <object; input> A data frame. Typically, same with paper. The columns for visits should be names "vis.INDICATOR-OF-VISITS".
#' @param mold <object; input> A data frame. Missing is 1, otherwise 0. The columns for visits should be names "miss.INDICATOR-OF-VISITS".
#' @param val.Visit <object; proccessing> A numeric vector. Indicators for Visits.
#'
#' @export

mf.SRIntporbyWideData <-
  function(
    paper = ADS.measurement,
    ink   = ADS.measurement,
    mold  = df.miss.measurement,
    val.Visit
  ){
    miss_which <-
      apply(
        test,
        MARGIN = 1,
        function(vec){
          pos_miss <- which(is.na(vec))
          if(length(pos_miss) > 0){
            if(pos_miss > 1 & pos_miss < length(vec)){
              vec[pos_miss] <- (vec[pos_miss-1]+vec[pos_miss+1])/2
              }
            }
          return(vec)
          }
        ) %>%
      t()

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
