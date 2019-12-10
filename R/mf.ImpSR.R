#' Interpolation.
#' @import  magrittr
#'
#' @param data
#'
#' @export

mf.SRIntporbyWideData <-
  function(
    data
    ){
    result <-
      apply(
        data,
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
    return(result)
  }
