# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")
library(patchwork)

all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/")
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/")
detected_tp <- readRDS("results_fredm/compile_tp_norev/detected_tp_lp.RDS")


data <- readRDS("data_fredm/byseries/CE16OV.RDS")[["2022"]]
plot(data)
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", tp_keep ="2001.16666666667",
                           nb_est = 8)
wrap_plots(all_plots, ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/ce16ov_fev2001_lp.pdf",
       plot = wrap_plots(all_plots, ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", tp_keep ="2020.25",
                           nb_est = 8)
wrap_plots(all_plots, ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/ce16ov_covid_lp.pdf",
       plot = wrap_plots(all_plots, ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

data <- readRDS("data_fredm/byseries/RETAILx.RDS")[["2022"]]
plot(window(data, start = 2019))
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           nb_est = 8)
wrap_plots(all_plots, ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/retailx_nov2007_lp.pdf",
       plot = wrap_plots(all_plots, ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

data <- readRDS("data_fredm/byseries/CE16OV.RDS")
data <- data[[length(data)]]
h <- lp_filter()@sfilter
final <- data * h
AQLTools::hc_stocks(window(ts.intersect(data, final),start = c(2018,1),
                           end = 2022))
