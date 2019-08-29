#' Distance measured by mutual information
#'
#'
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import tibble
#' @import GGally
#' @import infotheo
#'
#' @export

dist.mutualInfo <- function(
  data, var.subjID = subjID
  ){

  ADS <- data %>%
    mutate(
      subjID=as.factor(subjID),
      slideID=as.factor(slideID)
      )

  # ESSDAI subcomponents: ----------------------------------------------

  uid <- colnames(data)

  ADS <- data[
    ,
    c("subjID", uid)
    ]


  # Grid of subjects --------------------------------------------------------

  grid.Subj <- expand.grid(
    colnames(ADS)[c(-1,-18)],
    colnames(ADS)[c(-1,-18)],
    ADS$Type
    ) %>%
    mutate(
      Var1.num = as.numeric(Var1),
      #      gsub(pattern = "SS_",replacement = "",x = Var1)),
      Var2.num = as.numeric(Var2)
      #
    ) %>%
    filter(
      Var1.num <= Var2.num
    ) %>%

    unique()

  grid.Subj_bySubj <- expand.grid(
    colnames(ADS)[c(-1,-18)],
    colnames(ADS)[c(-1,-18)],
    ADS$Type,
    unique(ADS$subjID)
  ) %>%
    filter(
      as.numeric(Var1) <=
        as.numeric(Var2)
    ) %>%
    unique()

  grid.Subj_bySubj_HE <-
    grid.Subj_bySubj %>%
    filter(Var3=="HE")

  # calculation of mutual information (conditioned by subjID) using grid of pathologist ---------------------------------------------------

  res.CondMutInfo <- data.frame(
    ID=c(1,2,3),
    method=c("mm", "sg", "shrink")
  ) %>%
    ddply(
      .(method),
      function(df.method){
        res <- grid.Subj
        res$CondMutInfo <- apply(
          grid.Subj,
          1,
          function(D){
            X <- ADS[ADS$Type==as.character(D[3]),as.character(D[1])]
            Y <- ADS[ADS$Type==as.character(D[3]),as.character(D[2])]
            S <- ADS[ADS$Type==as.character(D[3]), 'subjID']
            print(df.method)
            print(head(data.frame(X,Y)))
            res <- try(
              infotheo::condinformation(
                X=X,
                Y=Y,
                S=S,
                method=df.method$method
              )
            )
            if(class(res)=="try-error"){res <- NA}
            return(res)
          }
        )
        return(res)
      }
    )

  res.MutInfo <- data.frame(
    ID=c(1,2,3),
    method=c("mm", "sg", "shrink")
  ) %>%
    ddply(
      .(method),
      function(df.method){
        res <- grid.Subj
        res$MutInfo <- apply(
          grid.Subj,
          1,
          function(D){
            X <- discretize(unlist(ADS[ADS$Type==as.character(D[3]),as.character(D[1])]))
            Y <- discretize(unlist(ADS[ADS$Type==as.character(D[3]),as.character(D[2])]))
            print(df.method)
            print(head(data.frame(X,Y)))
            res <- try(
              infotheo::mutinformation(
                X=X,
                Y=Y,
                method=df.method$method
              )
            )
            if(class(res)=="try-error"){res <- NA}
            return(res)
          }
        )
        return(res)
      }
    )

  gg.CondMutInfo <- ggplot(
    res.CondMutInfo %>%
      mutate(
        flg = as.factor(
          ifelse(Var1==Var2, "autoinfo", "mutuinfo"))
      ),
    aes(x=CondMutInfo)
  )
  gg.MutInfo <- ggplot(
    res.MutInfo %>%
      mutate(
        flg = as.factor(
          ifelse(Var1==Var2, "autoinfo", "mutuinfo"))
      ),
    aes(x=MutInfo)
  )
  plot(gg.CondMutInfo+geom_histogram(aes(fill=as.factor(flg)))+facet_grid(Var3+flg~method))
  plot(gg.MutInfo+geom_histogram(aes(fill=as.factor(flg)))+facet_grid(Var3+flg~method))


  # calculation of mutual information by subjects using grid of pathologist ---------------------------------------------------

  res.MutInfo_bySubj <- data.frame(
    ID=c(1,2,3),
    method=c("mm", "sg", "shrink")
  ) %>%
    ddply(
      .(method),
      function(df.method){
        res <- grid.Subj_bySubj_HE
        res$MutInfo <- apply(
          grid.Subj_bySubj_HE,
          1,
          function(D){
            X <- ADS[
              ADS$Type==as.character(D[3]) & ADS$subjID==as.character(D[4]),
              as.character(D[1])
              ]

            Y <- ADS[
              ADS$Type==as.character(D[3]) & ADS$subjID==as.character(D[4]),
              as.character(D[2])
              ]

            print(df.method)
            print(head(data.frame(X,Y)))
            res <- try(
              infotheo::mutinformation(
                X=X,
                Y=Y,
                method=df.method$method
              )
            )
            if(class(res)=="try-error"){res <- NA}
            return(res)
          }
        )
        return(res)
      }
    )


  summ.res.MutInfo_bySubj <-
    res.MutInfo_bySubj %>%
    ddply(
      .(method, Var1, Var2, Var3),
      function(D){
        mean <- mean(D$MutInfo,na.rm = TRUE)
      }
    ) %>%
    left_join(
      res.CondMutInfo,
      by=c("method", "Var1", "Var2", "Var3")
    ) %>%
    mutate(diff=CondMutInfo - V1) %>%
    ddply(
      .(method, Var3),
      function(D){
        D <- D %>%
          mutate(
            rank1 = rank(V1),
            rank2 = rank(CondMutInfo)
          )
      }
    )


  # ...... ------------------------------------------------------------------

  list.dist.ADS <- res.CondMutInfo %>%

    mutate(dist.MutInfo = max(res.CondMutInfo$CondMutInfo)-res.CondMutInfo$CondMutInfo) %>%
    dplyr::select(Var1, Var2, Var3, method,  dist.MutInfo) %>%
    # filter(!(Var1==Var2)) %>%     ## as.dist()
    dlply(
      .(Var3, method),
      function(D){
        D %>%
          spread(Var1, dist.MutInfo) %>%
          column_to_rownames("Var2") %>%
          dplyr::select(-Var3, -method) %>%
          as.dist()
      }
    )

}

