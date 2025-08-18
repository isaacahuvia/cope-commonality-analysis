install.packages("groundhog")
library(groundhog)
groundhog_date <- "2025-04-10"
meta.groundhog(groundhog_date)


pkgs <- c("tidyverse", "yhat", "boot", "car", "lmtest", "olsrr")
groundhog.library(pkgs, groundhog_date)