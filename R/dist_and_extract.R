#' Calculate distance and extract subjects
#'
#' @importFrom GUniFrac GUniFrac
#' @importFrom vegan vegdist
#' @import tibble
#' @import plyr
#' @import dplyr
#'
#' @param data A matrix to calculate distance
#' @param tree A tree object needed when method="UniFrac"
#' @param method Metric of distacnce.
#' @param type_extr Will be passed to ExploratoryDataAnalysis::extract_type function
#' @param Name_type Will be passeed to ExploratoryDataAnalysis::extract_type function
#'
#' @export
#'

dist_and_extract <- function(
  data,
  sufix = "extracted",
  tree_phylo=NULL,
  method="euclidean",
  type_extr="Type%in%c(3,4)",
  Name_type=df.Name_type,
  param.UniFrac= c(0, 0.5, 1.0, "UW") # -1 for unweighted UniFrac
  )
  {
  if(method=="UniFrac" & is.null(tree_phylo)){
    stop("Unifrac needs phylogenic tree")
    }

  OK_or_NoWay <-
    readline(
      "This function might overwrite objects in GlovalEnv. Are you OK? 'OK' or 'NW'"
      )
  if (OK_or_NoWay == "NW"){
    stop("STOP")
    }

  cat("OK\n")

  .otutable_on_taxonomy <- data

  .method    <- method
  .type_extr <- type_extr
  .Name_type <- Name_type


   if(length(which(param.UniFrac %in% "UW")>0)){
      .param.UniFrac <- as.numeric(
        param.UniFrac[
          -which(
            param.UniFrac %in% "UW"
            )
          ])
        .UW <- TRUE
        print(.param.UniFrac)
        } else {
         .param.UniFrac <- param.UniFrac
          .UW <- FALSE
          print(.param.UniFrac)
          }


  if(
    .method=="UniFrac" &
    length(.param.UniFrac) > 0
    ){
    unifrac_relative <- GUniFrac(
      .otutable_on_taxonomy,
      tree  = tree_phylo,
      alpha = .param.UniFrac
      )
    for(i in 1:length(.param.UniFrac)){
      assign(
        sprintf(
          "d%s_%s",
          .param.UniFrac[i],
          sufix
          ),
        unifrac_relative[[1]][
          ,,
          sprintf("d_%s", .param.UniFrac[i])
          ],
        envir = .GlobalEnv
        )
      }
    }
  print("hello1")
  if(
    .method %in% c("bray", "jaccard")
    ){

    res.dist <- vegdist(
      .otutable_on_taxonomy,
      method = .method
      ) %>%
      extract_type(
        extract_type,
        df.Name_type
        )
    print("hello2")

    assign(
      sprintf("d%s_vegan", .method),
      res.dist
      )
  }
}


