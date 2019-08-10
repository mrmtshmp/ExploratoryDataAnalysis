#' dist-class object manupulation: Extract subjects
#'
#' @import tibble
#' @import dplyr
#' @import plyr
#'
#' @param dist <object; input> dist-class object with IDs as rownames and colnames
#' @param Type_extr <character strings; proccessing> condition to extract, e.g., "!is.na(var_name)"
#' @param Name_type <object; input> A data.frame-class object with 2 column: IDs and the variable used to extract IDs
#'
#' @export

extract_type <- function(
  dist,
  Type_extr,
  Name_type
){
  if(length(colnames(dist))<2){
    stop("dist-class object must be named")
  }
  names <- Name_type %>%

    dplyr::filter(#parse(text=Type_extr))
       eval(parse(text=Type_extr))
       )

  row_output <- dist %>%
    as.matrix() %>%
    data.frame() %>%
    rownames_to_column("Name") %>%
    filter(
      Name %in% names$Name &
        Name != "Name"
      ) %>%
    dplyr::select(-Name)

  return(
    row_output[,names$Name] %>%
      as.matrix() %>%
      as.dist()
  )
}

#' @example
#'
#
# require(plyr)
# require(dplyr)
# require(tibble)
#
# df.Name_type <- data.frame(
#   "ID"  = LETTERS[1:10],
#   "Sex" = rep(c("F","M"), times=5),
#   "val" = matrix(c(1:30), ncol=3)
#   ) %>%
#   column_to_rownames("ID")
#
# mat.dist <- df.Name_type[
#   ,c("val.1","val.2","val.3")
#   ] %>%
#   dist(
#     method = "manhattan"
#     )
#
# Name_type <- data.frame(
#    "Name"  = rownames(df.Name_type),
#    "Sex"   = df.Name_type$Sex
#    )
#
# d00 <- extract_type(
#    mat.dist,
#    Type_extr="Sex=='F'",
#    Name_type=Name_type
#    )
