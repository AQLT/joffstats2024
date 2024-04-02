# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")

library(rjd3filters)
library(AQLThesis)


all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/")
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/")

detected_tp <- readRDS("results_fredm/compile_tp_norev/detected_tp_lp.RDS")


series <- "CE16OV"
tp_keep <- "2001.16666666667"
all_prevs <- get_all_prevs(series = series, tp_keep = tp_keep,
                           nb_est = 8, nb_dates_before = 6)
plots <- get_all_plots_prevs(data_prevs = all_prevs,
                             series = series,
                             all_tp = all_tp,
                             all_tp_rev = all_tp_rev,
                             tp_keep = tp_keep,
                             dossier = "results_fredm/compile_tp_norev/")
wrap_plots(plots,ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/ce16ov_fev2001_prev_imp_lp.pdf",
       plot = wrap_plots(plots, ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

series <- "CE16OV"
tp_keep <- "2020.25"
all_prevs <- get_all_prevs(series = series, tp_keep = tp_keep,
                           nb_est = 8, nb_dates_before = 6)
plots <- get_all_plots_prevs(data_prevs = all_prevs,
                             series = series,
                             all_tp = all_tp,
                             all_tp_rev = all_tp_rev,
                             tp_keep = tp_keep,
                             dossier = "results_fredm/compile_tp_norev/")
wrap_plots(plots,ncol = 3) & 
  scale_color_grey() & 
  labs(subtitle = NULL)

ggsave("paper/img/nber/ce16ov_covid_prev_imp_lp.pdf",
       plot = wrap_plots(plots, ncol = 3) & 
         scale_color_grey() & 
         labs(subtitle = NULL),
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

ggsave("paper/img/nber/retailx_nov2007_prev_imp_lp.pdf",
       plot = wrap_plots(plots, ncol = 3),
       width = 8, height = 8)
