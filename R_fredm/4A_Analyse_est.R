# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")
library(patchwork)

Sys.setlocale(locale = "en_US.UTF-8")
all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/")
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/")
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

all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", tp_keep ="2020.25",
                           nb_est = 8)
wrap_plots(all_plots, ncol =3)

ggsave("img/nber/ce16ov_covid_lp.pdf",
       plot = wrap_plots(all_plots, ncol = 3),
       width = 8, height = 8)

data <- readRDS("data_fredm/byseries/RETAILx.RDS")[["2020"]]
plot(window(data, start = 2019))
all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           nb_est = 8)
wrap_plots(all_plots, ncol = 3)

ggsave("img/nber/retailx_nov2007_lp.pdf",
            plot = wrap_plots(all_plots, ncol = 3),
            width = 8, height = 8)


c("CE16OV", "DPCERA3M086SBEA", "INDPRO", "PAYEMS", "RETAILx", 
  "W875RX1")
data <- readRDS("data_fredm/byseries/CE16OV.RDS")[["2022.75"]]
plot(window(data, start = 2020, end = 2021))
lapply(c("CE16OV", "DPCERA3M086SBEA", "INDPRO", "PAYEMS", "RETAILx", 
         "W875RX1"), function(x){
           readRDS("data_fredm/byseries/INDPRO.RDS")[["2020"]]
         })
