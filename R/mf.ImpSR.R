#' Interpolation.
#' @import  magrittr
#'
#' @param data A data.frame-class object.
#' @param col.visit A character vector.
#'
#' @export

mf.SRIntporbyWideData <-
  function(
    data, col.visit
    ){
    result <-
      apply(
        data,
        MARGIN = 1,
        function(vec){
          pos_miss <- which(is.na(vec))
          if(length(pos_miss) > 0){
            if(
              min(match(names(pos_miss),col.visit)) > 1 &
              max(match(names(pos_miss),col.visit)) < length(col.visit)
              ){
              vec[pos_miss] <-
                (as.numeric(vec[pos_miss-1]) +
                   as.numeric(vec[pos_miss+1])
                 )/2
              }
            }
          return(vec)
          }
        ) %>%
      t()
    result <- data.frame(result)
    print(head(result))
    for(i in 1:length(data)){
      class_i <- class(data[,i])
      print(sprintf("result[,i] <- as.%s(result[,i])", class_i))
      eval(
        parse(
          text = sprintf(
            "result[,i] <- as.%s(result[,i])", class_i)
          )
        )
      eval(
        parse(text=sprintf("print(result[,i])"))
        )
      print(class(result[,i]))
      }
    return(result)
  }
