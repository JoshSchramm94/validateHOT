# library(osfr)
# validatehot_data <- osfr::osf_retrieve_node("https://osf.io/m5x3a/")
# osfr::osf_ls_files(validatehot_data) %>%
#   dplyr::filter(name == "acbc_piecewise.csv") %>%
#   osfr::osf_download(path = "download/", conflicts = "overwrite")

library(tidyverse)

set.seed(1234)
acbc_interpolate <- read.csv("download/acbc_piecewise.csv") %>%
  dplyr::mutate(group = sample(c(1:3), replace = TRUE, size = nrow(.)))
usethis::use_data(acbc_interpolate, compress = "xz", overwrite = TRUE)
