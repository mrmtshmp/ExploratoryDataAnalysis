#' Preset parameters for plots to use ExploratoryDataAnalysis package.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param setID <numeric; preset=1>
#'
#' @export

## Note   :  including themes for ggplot and GGally

setting_plot <- function(setID=1){
  if(setID==1){
    summ_plot_geom <-  geom_boxplot()
    summ_hist_geom <-  geom_histogram()
    summ_point_geom <-  geom_point()

    summ_plot_facet <-  facet_wrap(
      ~var,
      ncol=4,
      # 自動だとエラー出る
      # 10Feb18: Age_reg 入れた時 "vapply(row_axes, is.zero, logical(length(row_axes))) でエラー: 値の長さは 2 でなければなりません、 しかし、FUN(X[[1]]) の結果の長さが 1 です "
      scales="free",
      strip.position="bottom"
    )
    summ_plot_facet_ncol3 <-  facet_wrap(
      ~var,
      ncol=3,
      # result の方は、ncol=4 だとエラー出る
      # 10Feb18: Age_reg 入れた時 "vapply(row_axes, is.zero, logical(length(row_axes))) でエラー: 値の長さは ３ でなければなりません、 しかし、FUN(X[[1]]) の結果の長さが 1 です "
      scales="free",
      strip.position="bottom"
    )
    summ_plot_facet_byVal <-  facet_wrap(
      ~var,
      ncol=3,
      scales="free",
      strip.position="bottom"
    )
    summ_plot_theme <- theme(
      legend.text = element_text(size = 20, colour = "black", angle = 45),
      strip.text.x = element_text(size =20, colour = "black", angle = 0),
      axis.text.x  = element_text(size =20, colour = "black", angle = 0, hjust=0, vjust=1),
      axis.text.y  = element_text(size =20, colour = "black", angle = 0, hjust=0, vjust=1),
      legend.background=element_blank(),
      strip.background = element_rect(colour="white", fill="white"),
      strip.placement = "outside",
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(colour="black", fill="white")
      )
    summ_plot_theme_byVal <- theme(
      legend.text = element_text(size = 20, colour = "black", angle = 45),
      strip.text.x = element_text(size =20, colour = "black", angle = 0),
      axis.text.x  = element_text(size =15, colour = "black", angle = 315, hjust=0, vjust=1),
      axis.text.y  = element_text(size =20, colour = "black", angle = 0, hjust=0, vjust=1),
      legend.background=element_blank(),
      strip.background = element_rect(colour="white", fill="white"),
      strip.placement = "outside",
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(colour="black", fill="white")
    )

    # histogram

    summ_hist_geom <-  geom_histogram(aes(fill=f.Sex, alpha=0.5), position="stack")
    summ_hist_facet <-  facet_wrap(~var,scales="free_x",strip.position="bottom")
    summ_hist_fill  <-  scale_fill_manual(values = c("#006699", "#990066"))
    summ_hist_theme <- theme(
      legend.text = element_text(size = 10, colour = "black", angle = 45),
      strip.text.x = element_text(size =5, colour = "black", angle = 0),
      legend.background=element_blank(),
      strip.background = element_rect(colour="white", fill="white"),
      strip.placement = "outside",
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(colour="black", fill="white")
    )

    # Scatter plot

    corr_plot_facet <-  facet_wrap(~f.Sex, scales="free",strip.position="bottom")

    corr_plot_theme <- theme(
      #  legend.text = element_text(size = 5, colour = "black", angle = 0),
      #  legend.position = c(5,7),
      #  legend.background=element_blank(),
      axis.text    = element_text(size =10, colour = "black", angle = 0),
      axis.ticks = element_blank(),
      strip.text.x = element_text(size =10, colour = "black", angle = 315, hjust=0, vjust=1),
      strip.text.y = element_text(size =10, colour = "black", angle = 125, hjust=1, vjust=0),
      strip.background = element_rect(colour="white", fill="white"),
      strip.placement = "outside",
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(colour="black", fill="white"))

    #  GGally::wrap_fn_with_arg(fun, params=list(...)) :
    #    ggpairs の 描画関数（ggally_points）の引数（params=内で指定） に値を渡す

    obj.wrap_fn_with_param_arg <- wrap_fn_with_param_arg(
      ggally_points,
      params=c(size=1)
      )
    obj.wrap_fn_with_param_arg <- wrap_fn_with_param_arg(
      ggally_box,
      params=c(size=0.5,linetype=1)
      )
    obj_2_.wrap_fn_with_param_arg <- wrap_fn_with_param_arg(
      ggally_box,
      params=c(size=0.5,linetype=1)
      )

    corr_plot_params <- list(
      continuous = obj.wrap_fn_with_param_arg,
      combo = obj.wrap_fn_with_param_arg
    )

    corr_plot_params_2 <- list(
      continuous = "barDiag",
      combo = obj_2_.wrap_fn_with_param_arg
      )

    shape_basket_1 <- c(8,15,16,17,18,19)
    shape_basket_2 <- c(9:14)

    plot.shape_1 <- scale_shape_manual(
      values=shape_basket_1
    )
    plot.shape_2 <- scale_shape_manual(
      values=shape_basket_2
      )
    }
  }


