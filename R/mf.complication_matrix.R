#' Complication matrix.
#'
#' @param data A data.frame-class object. Subjects' IDs have to be contained in a column named 'id'.
#' @param col_selector A regex string to specify names of cols to be analyzed.
#'
#' @export

mf.comlication_matrix <-
  function(
    data, col_selector='^sAB\\.[^others$]|^id$|^season$'
  ){
    ADS.sAB <-
      data[,
           grep(
             col_selector,
             colnames(data)
           )
      ] %>%
      gather(
        var, val,
        -id, -season
      )

    res.odds.sAB <-
      ddply(
        ADS.sAB,
        .(var),
        function(D){
          ids <- D[D$val==1,'id']
          d1 <-
            ADS.sAB[
              ADS.sAB$id %in% ids,
            ]
          res <-
            ddply(
              d1,
              .(var),
              function(d){
                return(
                  sum(as.numeric(d$val),na.rm = TRUE)
                )
              }
            )
          res <- data.frame(parent=unique(D$var), res)
          return(res)
        }
      ) %>%
      spread(var, V1)
    return(res.odds.sAB)
  }
