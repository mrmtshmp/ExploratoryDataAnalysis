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
    result <- data.frame(result,stringsAsFactors = FALSE)
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




#' Extrapolation the least change
#' @import  magrittr
#'
#' @param data A data.frame-class object.
#' @param col.visit A character vector.
#' @param ink.col.visit A character vector.
#' @param paper.col.vosot A character vector.
#' @param ADS.Saihi A data.frame-class object.
#' @param ADS.Demo A data.frame-class object.
#' @param .Set   A character vector.
#' @param val.Alloc A character vector
#'
#' @export


mf.LCExtrpbyWideData <-
  function(
    data, col.visit = c("vis.0","vis.6","vis.18"), ink.col.visit="vis.6", paper.col.visit="vis.18",
    ADS.Saihi, ADS.Demo, .Set = c("ITT","FAS","SAS","PPS"), val.Alloc=val.Alloc
    ){

    leastChange <-
      data %>%
      left_join(ADS.Saihi, by="SubjID") %>%
      dplyr::select(
        eval(
          setdiff(
            c(names(data), names(ADS.Saihi)),
            c(ink.col.visit, paper.col.visit)
          )
        )
      ) %>%
      gather(Set, Adopt, -SubjID, -Site) %>%
      filter(Set %in% .Set) %>%
      left_join(ADS.Demo[,c("SubjID","Alloc")], "SubjID") %>%
      filter(Adopt==1) %>%
      ddply(
        .(Set, Alloc),
        function(set){
          res <- set %>%
            left_join(data, "SubjID") %>%
            mutate( diff = get(paper.col.visit)-get(ink.col.visit))
          mindiff <- min( res$diff, na.rm = TRUE)
          return(mindiff)
        }
      ) %>%
      filter(Alloc %in% val.Alloc)

    result <-

      leastChange %>%
      ddply(
        .(Set, Alloc),
        function(x){
          print(x$V1)
          ADS <- data %>%
            left_join( ADS.Demo[,c("SubjID","Alloc")], "SubjID")

          test <- apply(
            ADS, 1,
            function(vec){
              # print(data.frame(t(vec))[c("Alloc", ink.col.visit, paper.col.visit)])
              vec["org.Alloc"] <- as.character(x[,"Alloc"])

              class.vec <- sapply(
                ADS,
                class
              )

              #            print(vec["SubjID"]); print(names(vec)[which(is.na(vec))]); print(vec)

              print(ink.col.visit %in% names(vec)[which(is.na(vec))])
              print(paper.col.visit %in% names(vec)[which(is.na(vec))])

              if(
                !(
                  ink.col.visit %in% names(vec)[which(is.na(vec))]
                ) &
                (paper.col.visit %in% names(vec)[which(is.na(vec))])
              ){
                print("IMP")
                vec[paper.col.visit] <- as.numeric(vec[ink.col.visit]) + as.numeric(x$V1[1])
              }
              return(vec)
            }
          )
          return(t(test))
        }
      )
  }
