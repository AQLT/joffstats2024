# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")

library(rjd3filters)
library(AQLThesis)

Sys.setenv(LANG = "en_US.UTF-8")
fst_weights <- "weight235"
fst_degree <- 2
all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/",
                     fst_weights = fst_weights,
                     fst_degree = fst_degree)
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/",
                         fst_weights = "weight235",
                         fst_degree = 2)

detected_tp <- readRDS("results_fredm/compile_tp_norev/detected_tp_lp.RDS")


series <- "CE16OV"
tp_keep <- "2001.16666666667"
all_prevs <- get_all_prevs(series = series, tp_keep = tp_keep,
                           nb_est = 8, nb_dates_before = 6,
                           fst_weights = fst_weights,
                           fst_degree = fst_degree)
plots <- get_all_plots_prevs(data_prevs = all_prevs,
                             series = series,
                             all_tp = all_tp,
                             all_tp_rev = all_tp_rev,
                             tp_keep = tp_keep,
                             dossier = "results_fredm/compile_tp_norev/",
                             fst_weights = fst_weights,
                             fst_degree = fst_degree)
wrap_plots(plots,ncol = 3)

ggsave("img/nber/ce16ov_fev2001_prev_imp_lp.pdf",
            plot = wrap_plots(plots, ncol = 3),
            width = 8, height = 8)


series <- "RETAILx"
tp_keep <- "2007.91666666667"

all_prevs <- get_all_prevs(series = series, tp_keep = tp_keep,
                           nb_est = 8, nb_dates_before = 6,
                           fst_weights = fst_weights,
                           fst_degree = fst_degree)
plots <- get_all_plots_prevs(data_prevs = all_prevs,
                             series = series,
                             all_tp = all_tp,
                             all_tp_rev = all_tp_rev,
                             tp_keep = tp_keep,
                             dossier = "results_fredm/compile_tp_norev/",
                             fst_weights = fst_weights,
                             fst_degree = fst_degree)
wrap_plots(plots, ncol = 3)

ggsave("img/nber/retailx_nov2007_prev_imp_lp.pdf",
            plot = wrap_plots(plots, ncol = 3),
            width = 8, height = 8)
