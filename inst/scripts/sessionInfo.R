library(cowplot)
library(devtools)
library(ggpubr)
library(here)
library(knitr)
library(rticles)
library(parallel)
library(sf)
library(spsurvey)
library(sptotal)
library(tidyverse)
library(viridis)
sessionInfo()
#> R version 4.0.3 (2020-10-10)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#>
#> Matrix products: default
#>
#> locale:
#> [1] LC_COLLATE=English_United States.1252
#> [2] LC_CTYPE=English_United States.1252
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C
#> [5] LC_TIME=English_United States.1252
#>
#> attached base packages:
#> [1] grid      parallel  stats     graphics  grDevices utils     datasets
#> [8] methods   base
#>
#> other attached packages:
#>  [1] viridis_0.6.1     viridisLite_0.4.0 forcats_0.5.1     stringr_1.4.0
#>  [5] dplyr_1.0.5       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3
#>  [9] tibble_3.1.2      tidyverse_1.3.1   sptotal_1.0.0     spsurvey_5.4.0
#> [13] survey_4.2        survival_3.2-7    Matrix_1.2-18     sf_1.0-1
#> [17] rticles_0.22      knitr_1.36        here_1.0.1        ggpubr_0.4.0
#> [21] ggplot2_3.3.5     devtools_2.4.0    usethis_2.0.1     cowplot_1.1.1
#>
#> loaded via a namespace (and not attached):
#>  [1] minqa_1.2.4        colorspace_2.0-1   ggsignif_0.6.3     deldir_1.0-0
#>  [5] ellipsis_0.3.2     class_7.3-17       rio_0.5.27         crossdes_1.1-1
#>  [9] rprojroot_2.0.2    fs_1.5.0           rstudioapi_0.13    proxy_0.4-25
#> [13] remotes_2.4.1      lubridate_1.7.10   fansi_0.5.0        xml2_1.3.2
#> [17] splines_4.0.3      cachem_1.0.4       pkgload_1.2.1      jsonlite_1.7.2
#> [21] nloptr_1.2.2.2     broom_0.7.6        dbplyr_2.1.1       httr_1.4.2
#> [25] compiler_4.0.3     sampling_2.9       backports_1.2.1    assertthat_0.2.1
#> [29] fastmap_1.1.0      cli_2.5.0          htmltools_0.5.1.1  prettyunits_1.1.1
#> [33] tools_4.0.3        gtable_0.3.0       glue_1.4.2         Rcpp_1.0.7
#> [37] carData_3.0-4      cellranger_1.1.0   styler_1.5.1       vctrs_0.3.8
#> [41] nlme_3.1-149       xfun_0.23          ps_1.6.0           rvest_1.0.0
#> [45] openxlsx_4.2.4     testthat_3.0.2     lme4_1.1-27.1      lpSolve_5.6.15
#> [49] lifecycle_1.0.0    gtools_3.8.2       rstatix_0.7.0      MASS_7.3-53
#> [53] scales_1.1.1       hms_1.0.0          yaml_2.2.1         curl_4.3.1
#> [57] gridExtra_2.3      memoise_2.0.0      stringi_1.5.3      highr_0.9
#> [61] AlgDesign_1.2.0    desc_1.3.0         e1071_1.7-6        boot_1.3-25
#> [65] pkgbuild_1.2.0     zip_2.1.1          rlang_0.4.11       pkgconfig_2.0.3
#> [69] evaluate_0.14      lattice_0.20-41    processx_3.5.2     tidyselect_1.1.1
#> [73] magrittr_2.0.1     R6_2.5.1           generics_0.1.0     DBI_1.1.1
#> [77] pillar_1.6.1       haven_2.4.1        foreign_0.8-80     withr_2.4.2
#> [81] units_0.7-2        abind_1.4-5        modelr_0.1.8       crayon_1.4.1
#> [85] car_3.0-11         KernSmooth_2.23-17 utf8_1.2.1         rmarkdown_2.13
#> [89] readxl_1.3.1       data.table_1.14.0  callr_3.7.0        reprex_2.0.0
#> [93] digest_0.6.27      classInt_0.4-3     munsell_0.5.0      mitools_2.4
#> [97] sessioninfo_1.1.1
