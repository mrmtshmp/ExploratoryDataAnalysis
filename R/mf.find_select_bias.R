#' Complication matrix.
#'
#' @param data A data.frame-class object. Subjects' IDs have to be contained in a column named 'id'.
#' @param col_selector A regex string to specify names of cols to be analyzed.
#'
#' @export

mf.find_select_bias <-

  function(
    obj.rpart,
    Data,
    colnames.targ,
    threshPred = 0.5
    ){

    for(
      i.factor in 1:length(colnames.targ)
      ){

      correct <- obj.rpart$terms[[2]]

      Data$factor  <- Data[,colnames.targ[i.factor]]
      Data$correct <- obj.rpart$y
      Data$test <-
        as.numeric(predict(obj.rpart, Data)[,2] > threshPred) + 1

      if(class(Data$factor)!= "factor")
        Data$factor <- factor(
          Data$factor,
          levels = unique(Data$factor)[order(unique(Data$factor))]
        )

      Data_factor_0 <- Data[Data$factor == levels(Data$factor)[1],]
      Data_factor_1 <- Data[Data$factor == levels(Data$factor)[2],]

      res.fisher.factor_0 <-
        try(
          fisher.test(
            table(
              Data_factor_0$test,
              Data_factor_0$correct
            )
          )$estimate
        )

      res.fisher.factor_1 <-
        try(
          fisher.test(
            table(
              Data_factor_1$test,
              Data_factor_1$correct
            )
          )$estimate
        )

      res.fisher.total <-
        fisher.test(
          table(
            Data$test,
            Data$correct
          )
        )$estimate

      if(class(res.fisher.factor_1)!='try-error') {
        result.i <-
          data.frame(
            Bias=res.fisher.factor_1/res.fisher.total,
            `OR at strata 0`=res.fisher.factor_0,
            `OR at strata 1`=res.fisher.factor_1,
            Total=res.fisher.total
          )
        result.i$factor <- colnames.targ[i.factor]
        if('result' %in% ls()) result <- bind_rows(result, result.i)
        if(!('result' %in% ls())) result <- result.i
      }
    }
    return(result)
  }
