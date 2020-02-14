#' Create table of summary statistics for categorical variables.
#'
#' @importFrom dplyr rename
#' @importFrom magrittr %>%
#'
#' @param df.input <object; input> A data.frame-class object with column of var.x, var.y, str.
#' @param ADS A data.frame-class object of analysis dataset.
#'
#' @export


T1_for_factorVar <- function(df.input, ADS){
  result <- df.input %>%
    filter(var.type=="factor") %>%
    ddply(
      .(var.x, str),
      function(D){
        if(D$var.type=="factor"){
          count <-
            table(
              as.factor(ADS[,D$var.x]),
              as.factor(ADS[,D$str]),
              dnn = c("var.x value", "str value")
            ) %>%
            data.frame() %>%
            mutate(
              Prop = round(Freq/ sum(Freq), 3)
            )
          print(length(unique(ADS[,D$str])))

          if(length(unique(ADS[,D$str]))>1 &length(unique(ADS[,D$var.x]))>1){
            res.fisher.test <- fisher.test(
              as.factor(ADS[,D$var.x]),
              as.factor(ADS[,D$str])
            )
            p.val.fisher.test <- res.fisher.test$p.value
            names(p.val.fisher.test) <- "p.val.Fisher_test"
          }else{
            p.val.fisher.test <- NA
          }
          count[  , "p.val.Fisher_test"] <- ""
          count[ 1, "p.val.Fisher_test"] <- as.character(p.val.fisher.test)
          res <- count
        }
        return(res)
      }
    )
  return(result)
}

#' Create table of summary statistics for continuous variables.
#'
#' @importFrom dplyr rename
#' @importFrom magrittr %>%
#'
#' @param df.input <object; input> A data.frame-class object with column of var.x, var.y, str.
#' @param ADS A data.frame-class object of analysis dataset.
#'
#' @export


T1_for_numVar <- function(df.input, ADS){
  result <-
    df.input %>%
    filter(
      var.type == "num"
    ) %>%
    ddply(
      .(var.x, str),
      function(D){
        if(D$var.type == "num"){
          ADS$strDumm <- ADS[,D$str]
          summary_tab <-
            ADS %>%
            ddply(
              .(strDumm),
              function(ADSsub){
                res <-
                  data.frame(
                    n   = sum(!is.na(ADSsub[,D$var.x])),
                    Ave = mean(ADSsub[,D$var.x], na.rm=TRUE),
                    Sd  = sd(ADSsub[,D$var.x], na.rm=TRUE),
                    Qvec= t(quantile(ADSsub[,D$var.x], na.rm=TRUE))
                  )
                return(res)
              }
            )
          return(summary_tab)
        }
      }
    )
  result <- result %>% dplyr::rename("str.value"= "strDumm")
  return(result)
}
