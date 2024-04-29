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
legende <- c(lc = "LC",
             lc_localic_final_h6_d2 = "LC loc. param.\n(final estimates)",
             lc_localic_h6_d2 = "LC loc.\nparam.",
             ql = "QL", ql_localic_final_h6_d2 = "QL loc. param.\n(final estimates)",
             ql_localic_h6_d2 = "QL loc.\nparam.",
             cq = "CQ", daf = "DAF",
             auto_arima = "ARIMA")
wrap_plots(all_plots[names(legende)], ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/ce16ov_fev2001_lp.pdf",
       plot = wrap_plots(all_plots[names(legende)], ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", 
                           tp_keep ="2020.25", tp_plot ="2020.25",
                           nb_est = 8,
                           vline = FALSE)
p <- wrap_plots(all_plots[names(legende)], ncol = 3) & scale_color_grey() &
  geom_vline(xintercept = detected_tp[detected_tp$series == series,paste0("X", "2020.25")], 
             linetype = "dotted") &
  labs(subtitle = NULL)

ggsave("paper/img/nber/ce16ov_covid_lp.pdf",
       plot = p,
       width = 8, height = 8)

data <- readRDS("data_fredm/byseries/RETAILx.RDS")[["2022"]]
plot(window(data, start = 2019))
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           nb_est = 8)
wrap_plots(all_plots[names(legende)], ncol = 3) & scale_color_grey()

ggsave("paper/img/nber/retailx_nov2007_lp.pdf",
       plot = wrap_plots(all_plots[names(legende)], ncol = 3) & scale_color_grey(),
       width = 8, height = 8)

