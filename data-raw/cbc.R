# library(osfr)
# validatehot_data <- osfr::osf_retrieve_node("https://osf.io/m5x3a/")
# osfr::osf_ls_files(validatehot_data) %>%
#   dplyr::filter(name == "cbc_partworth.csv") %>%
#   osfr::osf_download(path = "download/", conflicts = "overwrite")

library(tidyverse)

set.seed(1234)
cbc <- read.csv("download/cbc_partworth.csv") %>%
  dplyr::mutate(group = sample(c(1:3), replace = TRUE, size = nrow(.)))
usethis::use_data(cbc, compress = "xz", overwrite = TRUE)
