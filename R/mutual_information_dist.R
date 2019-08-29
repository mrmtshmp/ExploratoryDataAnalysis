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



pdf(
  sprintf("%s/%s.pdf", dir.output, "HierClust_CondMutInf")
  )
llply(
  list.dist.ADS,
  function(dist){
    plot(hclust(dist, method = "ward.D"))
    }
  )
dev.off()


# Dendrogram for dist.ADS ----------------------------------

dist.ADS <- list.dist.ADS[[1]]

require(ape)

if(!require("dendextend")){install.packages("dendextend");require("dendextend")}
if(!require("circlize")){install.packages("circlize");require("circlize")}

test <-
  list.dist.ADS %>%
    llply(
      function(dist.ADS){
        # create a dendrogram
        hc   <- hclust(dist.ADS, "ward.D")
        dend <- as.dendrogram(hc)

        # modify the dendrogram to have some colors in the branches and labels
        plot.dend <- dend %>%
          color_branches(h=1) #%>%
          # dendextend::set(
          #   "branches_k_color",
          #   value = 7:1, k = 7
          # ) #%>%
          #plot(
          #  main = "Controlling branches' colors\n(via clustering)",
          #  horiz=T
          #)
        return(plot.dend)
        # colored_bars(
        #   colors = the_bars,
        #   dend =
        #     dend,
        #   # rowLabels =
        #   #   c("IgG_np", "ACA_np"),
        #   y_shift = 0.03,
        #   horiz = T
        # )
        # legend(
        #   "topright", legend = c('0', '1', 'NA'), pch = 15, pt.cex = 3, cex = 1.5, bty = 'n',
        #   inset = c(0, 0), # place outside
        #   title = "Status",
        #   col = c('beige', 'firebrick3', 'gray')
        # )
        }
      )

pdf(
  sprintf(
    "%s/%s.pdf", dir.output, "test"
    ),
  width = 10, height = 7,
  bg="white"
  )
plot(test[[1]], horiz=T)
plot(test[[2]], horiz=T)
plot(test[[3]], horiz=T)
plot(test[[4]], horiz=T)
plot(test[[5]], horiz=T)
plot(test[[6]], horiz=T)
dev.off()



# plot the radial plot
par(mar = rep(7,4), bg="white", xpd=NA)
# circlize_dendrogram(dend, dend_track_height = 0.8)
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .7)


the_bars_IgG <- ifelse(
  is.na(ADS$IgG_np), "gray",
  ifelse(ADS$IgG_np, "firebrick3", "beige")
  )

the_bars <- ifelse(
  is.na(ADS[,uid]), "gray",ADS[,uid]
  )


# Add the legend manually


pdf(
  sprintf("%s/%s.pdf", dir.output, "test"), width = 30, height = 40,
  bg="lightgray"
  )
dend %>%
  dendextend::set(
    "branches_k_color",
    value = 7:1, k = 7
    ) %>%
  plot(
    main = "Controlling branches' colors\n(via clustering)",
    horiz=T
    )

colored_bars(
  colors = the_bars,
  dend =
    dend,
  # rowLabels =
  #   c("IgG_np", "ACA_np"),
  y_shift = 0.03,
  horiz = T
  )
legend(
  "topright", legend = c('0', '1', 'NA'), pch = 15, pt.cex = 3, cex = 1.5, bty = 'n',
  inset = c(0, 0), # place outside
  title = "Status",
  col = c('beige', 'firebrick3', 'gray')
  )
dev.off()
# Dendrogram for dist.ADS_exc.Biological ----------------------------------
# create a dendrogram
hc <- hclust(dist.ADS_exc.Biological)
dend <- as.dendrogram(hc)

# modify the dendrogram to have some colors in the branches and labels
dend <- dend %>%
  color_branches(k=10) %>%
  color_labels

# plot the radial plot
par(mar = rep(0,4))
# circlize_dendrogram(dend, dend_track_height = 0.8)
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .7)

dend %>% set("branches_k_color", value = 3:1, k = 3) %>%
  plot(main = "Controlling branches' colors\n(via clustering)")

plot(as.phylo(hclust(dist.ADS)),type="fan")
plot(as.phylo(hclust(dist.ADS_exc.Biological)),type="fan")


pdf(
  sprintf("%s/%s",dir.output,"PCoA_MutInfo.pdf"),
  width=10, height=7
  )

llply(
  list.dist.ADS,
  function(mat.dist){
    ExploratoryDataAnalysis::gg_PCoA(
      mat.dist,
      as.factor(
        colnames(ADS)[c(2:17)]
        ),
      overlay=TRUE,
      legend.name = "pthlgst.ID",
      jitter.v = 0.03, jitter.h = 0.03,
      alpha = 0.7
      )
    }
  )
dev.off()


pdf(
  sprintf("%s/%s",dir.output,"PCoA_MutInfo_exc.Biological.pdf"),
  width=10, height=7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$FS > 1
    ),
  overlay=FALSE,
  size=2,
  legend.name = "FS > 1",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$FS
  ),
  overlay=FALSE,
  size=2,
  legend.name = "FS",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$ACA_np
    ),
  overlay=FALSE,
  size=2,
  legend.name = "ACA",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$IgG_np
  ),
  overlay=FALSE,
  size=2,
  legend.name = "IgG",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )
ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$RF_np
  ),
  overlay=FALSE,
  size=2,
  legend.name = "RF",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$Saxon_np
    ),
  overlay=FALSE,
  legend.name = "Saxon test",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$ANA_type
  ),
  overlay=FALSE,
  legend.name = "ANA",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )

ExploratoryDataAnalysis::gg_PCoA(
  dist.ADS_exc.Biological,
  as.factor(
    ADS$ANA_num_strict
  ),
  overlay=FALSE,
  legend.name = "ANA",
  jitter.v = 0.03, jitter.h = 0.03,
  alpha = 0.7
  )
dev.off()


# Tilling plot ------------------------------------------------------------




pdf("Tilling_ManhattanWard.D2.pdf",width=10, height=7)



func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=3,"≥ 3"," 1, 2")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq3"
  )


func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=4,"≥ 4"," 1, 2 & 3")
    )%>%
    dplyr::select( ID, US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq4"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=5,"≥ 5"," 1, 2, 3 & 4")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq5"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=6,"≥ 6"," 1 ~5")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq6"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=7,"≥ 7"," 1 ~7")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq7"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(FS >=8,"≥ 8"," 1 ~8")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "FS score ",
  fn.output.pdf = "tile.FS_geq8"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(
        !is.na(US_score),
        ifelse(
          US_score>=2,"≥ 2","< 2"
          ),
        "NA"
        )
      )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq2"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=3,"≥ 3","< 3")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq3"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=4,"≥ 4","< 4")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq4"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=5,"≥ 5","< 5")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq5"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=6,"≥ 6","< 6")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq6"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=7,"≥ 7","< 7")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq7"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=8,"≥ 8","< 8")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq8"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=9,"≥ 9","< 9")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq9"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=10,"≥ 10","< 10")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq10"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=11,"≥ 11","< 11")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq11"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$US_score),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    mutate(
      US_class = ifelse(US_score>=12,"≥ 12","< 12")
    )%>%
    dplyr::select(ID,US_class),
  surfix.maintitle = "US score ",
  fn.output.pdf = "tile.USscore_geq10"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$FS),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    filter(!is.na(FS)) %>%
    mutate(
      FS_class = ifelse(FS>=1,"≥ 1","< 1")
      )%>%
    dplyr::select(ID,FS_class),
  surfix.maintitle = "FS ",
  fn.output.pdf = "tile.FS_geq1"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$FS),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    filter(!is.na(FS)) %>%
    mutate(
      FS_class = ifelse(FS>=2,"≥ 2","< 2")
    )%>%
    dplyr::select(ID,FS_class),
  surfix.maintitle = "FS ",
  fn.output.pdf = "tile.FS_geq2"
  )

func.tilemap(
  count_table = ADS[!is.na(ADS$FS),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    filter(!is.na(FS)) %>%
    mutate(
      FS_class = ifelse(FS>=1,"≥ 1","< 1")
    )%>%
    dplyr::select(ID,FS_class),
  surfix.maintitle = "FS ",
  fn.output.pdf = "tile.FS_geq"
  )


func.tilemap(
  count_table = ADS[!is.na(ADS$FS),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    filter(!is.na(FS)) %>%
    mutate(
      FS_class = ifelse(FS==1,"= 1", ifelse(FS>=2,">= 2",NA))
    )%>%
    dplyr::select(ID,FS_class),
  surfix.maintitle = "FS ",
  fn.output.pdf = "tile.FS_1_geq2"
)

func.tilemap(
  count_table = ADS[!is.na(ADS$ACA_np),c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    dplyr::select(ID,ACA_np) %>%
    filter(!is.na(ACA_np)),
  surfix.maintitle = "ACA: ",
  fn.output.pdf = "tile.ACA_np"
  )

func.tilemap(
  count_table = ADS[,c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    dplyr::select(ID,RF_np),
  surfix.maintitle = "RF: ",
  fn.output.pdf = "tile.RF"
  )

func.tilemap(
  count_table = ADS[,c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    dplyr::select(ID,Saxon_np),
  surfix.maintitle = "Saxon test: ",
  fn.output.pdf = "tile.Saxon"
  )
dev.off()

func.tilemap(
  count_table = ADS[,c(ESSDAI_sc)],
  physi_table = ADS %>%
    rownames_to_column('ID') %>%
    dplyr::select(ID,RF_np),
  surfix.maintitle = "RF ",
  fn.output.pdf = "tile.RF"
)
dev.off()

