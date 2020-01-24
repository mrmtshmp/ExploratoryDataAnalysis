#' Make many boxplots from tidy ordering sheet
#'
#' @import dplyr
#' @import coin
#'
#' @param df.ADS <object; input> A data frame
#' @param df.input_for_cmp_2levels <object; input> A data frame with variables (var.x, var.y, str)
#'
#' @export

wilcox_test_auto_binomialier <- function(
  df.ADS,
  df.input_for_cmp_2levels
  ){
  df.input.cmpr.AlphaDiv.by_variables_in_MVA <-

    df.input_for_cmp_2levels %>%
    ddply(
      .(var.x, var.y, str),
      function(D){
        select.2_levels <-
          expand.grid(
            unique(df.ADS[,D$var.x]),
            unique(df.ADS[,D$var.x])
          ) %>%
          dplyr::filter(Var1 < Var2) %>%
          data.frame()
        return(select.2_levels)
      }
    )

  cmpr.AlphaDiv.by_variables_in_MVA <-
    df.input.cmpr.AlphaDiv.by_variables_in_MVA %>%
    dlply(
      .(var.x, var.y, str, Var1, Var2),
      function(LEVELS){
        print(LEVELS)
        df.ADS <- df.ADS[
          df.ADS[,LEVELS$var.x] %in%
            c(
              as.character(LEVELS$Var1),
              as.character(LEVELS$Var2)
            ),
          ]

        fml.Wilcox <- sprintf(
          "%s ~ as.factor(%s)",
          as.character(LEVELS$var.y),
          as.character(LEVELS$var.x)
        )
        res <-
          coin::wilcox_test(
            formula =
              as.formula(fml.Wilcox),
            data =
              df.ADS %>%
              filter(
                eval(parse(text =
                             sprintf("%s==1", LEVELS$str)
                )
                )
              )
          )
        res <- c(sprintf("filtered by %s==1", LEVELS$str), res)
        return(unlist(res))
      }
    )
}
