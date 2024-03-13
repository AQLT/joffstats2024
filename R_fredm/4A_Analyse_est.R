# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")
library(patchwork)


fst_weights = "weight235"
fst_degree = 2
all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/",
                     fst_weights = "weight235",
                     fst_degree = 2)
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/",
                     fst_weights = "weight235",
                     fst_degree = 2)
detected_tp <- readRDS("results_fredm/compile_tp_norev/detected_tp_lp.RDS")




data <- readRDS("data_fredm/byseries/CE16OV.RDS")[["2020"]]
dput(round(data, 3))
plot(data)
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", tp_keep ="2001.16666666667",
                           nb_est = 8)
wrap_plots(all_plots, ncol =3)

ggsave("img/nber/ce16ov_fev2001_lp.pdf",
            plot = wrap_plots(all_plots, ncol = 3),
            width = 8, height = 8)

data <- readRDS("data_fredm/byseries/RETAILx.RDS")[["2020"]]
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           nb_est = 8)
wrap_plots(all_plots, ncol = 3)

ggsave("img/nber/retailx_nov2007_lp.pdf",
            plot = wrap_plots(all_plots, ncol = 3),
            width = 8, height = 8)
