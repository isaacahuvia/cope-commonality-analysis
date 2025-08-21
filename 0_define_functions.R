# Define an adaptation of "yhat" package's function "boot.yhat()" to optionally
# take a "prec" argument that can be passed to "calc.yhat()"
# - This is so that when running "booteval.yhat(prec = D)" the output will actually 
#   show results rounded to the D decimal places that the user specifies
# - This is achieved by running "booteval.yhat()" on the results from "boot(statistic 
#   = boot.yhat.prec, prec = D)" instead of from "boot(statistic = boot.yhat)"
# - Link to original "boot.yhat()":
#   https://github.com/cran/yhat/blob/3beef32b280f86b63c3a7cc4ad08d08a4ad3d4a7/R/boot.yhat.r

boot.yhat.prec <- function (data, indices, lmOut, regrout0, prec = NULL)
{
  data <- data[indices, ]
  blmOut <- lm(formula = lmOut$call$formula, data = data)
  
  if (!is.null(prec)) {
    regrout <- calc.yhat(blmOut, prec)
  } else {
    regrout <- calc.yhat(blmOut)
  }
  
  pm <- as.vector(regrout$PredictorMetrics[-nrow(regrout$PredictorMetrics), 
  ])
  apsm <- as.vector(as.matrix(regrout$APSRelatedMetrics[-nrow(regrout$APSRelatedMetrics), 
                                                        -2]))
  pdm <- as.vector(as.matrix(regrout$PairedDominanceMetrics))
  tau <- vector(length = ncol(regrout$PredictorMetrics))
  for (i in 1:length(tau)) {
    s1 <- (regrout0$PredictorMetrics[1:(nrow(regrout0$PredictorMetrics) - 
                                          1), i])
    s2 <- (regrout$PredictorMetrics[1:(nrow(regrout$PredictorMetrics) - 
                                         1), i])
    tau[i] <- cor.test(s1, s2, method = "kendall", exact = FALSE)$estimate
  }
  c(pm, pdm, apsm, tau)
}