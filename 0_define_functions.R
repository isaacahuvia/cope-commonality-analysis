# Define an adaptation of "yhat" package's function "boot.yhat()" to use "prec = 6"
# in "calc.yhat()" instead of the "calc.yhat()" default of "prec = 3"
# - This is so that when running "booteval.yhat(prec = 6)" the output will actually 
#   show results rounded to 6 decimal places
# - This is achieved by running "booteval.yhat()" on the results from "boot(statistic 
#   = boot.yhat.prec6)" instead of on the results from "boot(statistic = boot.yhat)"
# - Link to original "boot.yhat()":
#   https://github.com/cran/yhat/blob/3beef32b280f86b63c3a7cc4ad08d08a4ad3d4a7/R/boot.yhat.r

boot.yhat.prec6 <- function (data, indices, lmOut, regrout0, prec = 6) 
{
  data <- data[indices, ]
  blmOut <- lm(formula = lmOut$call$formula, data = data)
  regrout <- calc.yhat(blmOut, prec)
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
