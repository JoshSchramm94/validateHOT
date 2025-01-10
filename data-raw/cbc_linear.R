# library(osfr)
# validatehot_data <- osfr::osf_retrieve_node("https://osf.io/m5x3a/")
# osfr::osf_ls_files(validatehot_data) %>%
#   dplyr::filter(name == "cbc_linear.csv") %>%
#   osfr::osf_download(path = "download/", conflicts = "overwrite")

library(tidyverse)

set.seed(1234)
cbc_linear <- read.csv("download/cbc_linear.csv") %>%
  dplyr::mutate(group = sample(c(1:3), replace = TRUE, size = nrow(.)))
usethis::use_data(cbc_linear, compress = "xz", overwrite = TRUE)
