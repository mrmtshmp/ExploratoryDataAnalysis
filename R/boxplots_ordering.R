#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param ggdata <object; input> A object with ggplot-class
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param str <character; proccessing>
#' @param dn.surfix <character; output>
#'
#' @export

# Box plot -----------

mf.boxplot <- function(

  data,
  ggdata,
  var.x,
  var.y,
  size = 0.5,
  var.col=NA,
  str,
  dn.surfix
  ){

  formula.facet <- sprintf("%s ~ %s", ".", str)

  n.str <- length(
    t(
      unique(data[,str])
    )
  )

  pdf(
    sprintf(
      "%s/%s.pdf",
      dir.output,
      dn.surfix
    ),
    width = 5 * n.str
  )

  if(
    !is.na(var.col)
    ) {
    plot.color <-
      scale_color_gradient(low = "blue", high = "orange")

    jitter <- geom_jitter(
      aes(
        y   = get(var.y),
        x   = get(var.x),
        color=get(var.col)
      ),
      size = size,
      width = 0.3
      )
    }else{
      plot.color <-
        scale_color_gradient(low = "black", high = "black")

      jitter <- geom_jitter(
        aes(
          y   = get(var.y),
          x   = get(var.x)
          ),
        size = size,
        width = 0.3
        )
      }

  plot(
    plot.box_plot <-
      ggdata +
      geom_boxplot(
        aes(
          y   = get(var.y),
          x   = get(var.x)
        ),
        color="gray",
        outlier.alpha = 0
      ) +

      jitter +


      plot.color +
      scale_y_log10() +
      facet_grid(
        as.formula(formula.facet)
        ) +
      theme_bw()
  )

  dev.off()
}


#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#'
#' @param D <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param data <object;> A data.frame-class object
#' @param ggdata <object; input> A object with ggplot-class
#'
#' @export

mf.wrap.boxplot <- function(D, data, ggdata){
  .var.x     = D$var.x
  .var.y     = D$var.y
  .var.col   = D$var.col
  .str       = D$str
  .dn.surfix = D$dn.surfix
  mf.boxplot(data, ggdata, var.x=.var.x, var.y=.var.y, var.col=.var.col, str=.str, dn.surfix = .dn.surfix)
  }


#' Make many scatterplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param ggdata <object; input> A object with ggplot-class
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param str <character; proccessing>
#' @param dn.surfix <character; output>
#' @param beta <character; proccessing> A numeric vector for coefficients of regression line
#'
#' @export

# Scatter plot -------

mf.scatterplot <- function(
  data, ggdata, var.x, var.y,
  trans.y=c("log10", "identity"),
  trans.x=c("log10", "identity"),
  var.col, cont.col, str, dn.surfix, betas
){

  formula.facet <- sprintf(
    "%s ~ %s", ".",
    str
  )

  if(cont.col == TRUE){
    scale_colour <-
      scale_color_gradient(
        low  = "blue",
        high = "orange"
      )
  } else
  {
    scale_colour <-
      scale_color_brewer(
        type="qual",
        palette = "Dark2"
      )
  }

  trans.y <- trans.y
  trans.x <- trans.x
  n.str <- length(
    t(
      unique(data[,str])
    )
  )
  print(sprintf("strata=%s", n.str))
  pdf(
    sprintf(
      "%s/%s.pdf",
      dir.output,
      dn.surfix
    ),
    width = 7 * n.str
  )

  p =
    ggdata +
    geom_point(
      aes(
        y   = get(var.y),
        x   = get(var.x),
        col = get(var.col)
      )
    ) +

    geom_line(
      aes(
        y = exp(
          betas[1] +
            x * betas[2] +
            str * betas[3] +
            x * str * betas[4]
        )
      )
    ) +

    facet_grid(
      as.formula(formula.facet)
    ) +

    scale_colour

  if(
    (trans.x=="NoScale") &
    (trans.y=="NoScale")
  ){
    print("No scaled")
    plot(p)
  }

  if(
    (trans.x=="NoScale") &
    (trans.y!="NoScale")
  ){
    print(" Y scaled")
    plot(
      p +
        scale_y_continuous(trans=trans.y)
    )
  }

  if(
    (trans.x!="NoScale") &
    (trans.y=="NoScale")
  ){
    print(" X scaled")
    plot(
      p +
        scale_x_continuous(trans=trans.x)
    )
  }

  if(
    (trans.x!="NoScale") &
    (trans.y!="NoScale")
  ){
    print("Both scaled")
    plot(
      p +
        scale_y_continuous(trans=trans.x) +
        scale_x_continuous(trans=trans.x)
    )
  }

  dev.off()
}

#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#'
#' @param D <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param data <object;> A data.frame-class object
#' @param ggdata <object; input> A object with ggplot-class
#'
#' @export

mf.wrap.scatterplot <- function(D, data, ggdata){
  .var.x     = D$var.x
  .var.y     = D$var.y
  .trans.y   = D$trans.y
  .trans.x   = D$trans.x
  .var.col   = D$var.col
  .str       = D$str
  .dn.surfix = D$dn.surfix
  .cont.col  = 1 - (is.factor(.var.col) + is.character(.var.col))
#  .betas     = D[, c('b0','b1','b2','b3')]

  mf.scatterplot(
    data , ggdata,
    .var.x, .var.y, .trans.y, .trans.x,
    .var.col, .cont.col, .str, .dn.surfix#,
#    beatas=.betas
  )
}

