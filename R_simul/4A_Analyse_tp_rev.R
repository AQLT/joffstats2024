source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(latex2exp)

tp_lp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
               readRDS("results_simul/compile_tp/peaks_lp.RDS"),
               by=c("series","kernel", "method")) %>%
  select_var()

tp_lic_final <- merge(readRDS("results_simul/compile_tp/troughs_localic_final.RDS"),
                      readRDS("results_simul/compile_tp/peaks_localic_final.RDS"),
                      by=c("series", "kernel", "h", "degree", "method"))  %>%
  # dplyr::filter() %>%
  # mutate(method = ifelse(degree == "d2", h == "h6"))
  select_var() %>%
  mutate(method = sprintf("%s_localic_final_%s_%s", method, degree, h)) %>%
  select(!c(degree, h))

tp_lic_daf_trunc <- merge(
  readRDS("results_simul/compile_tp/troughs_localic_daf_trunc.RDS"),
  readRDS("results_simul/compile_tp/peaks_localic_daf_trunc.RDS"),
  by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var() %>%
  mutate(method = sprintf("%s_localic_%s_%s", method, degree, h)) %>%
  select(!c(degree, h))


tp_lp_nn <- merge(readRDS("results_simul/compile_tp/troughs_lp_nn.RDS"),
                  readRDS("results_simul/compile_tp/peaks_lp_nn.RDS"),
                  by=c("series","kernel", "method")) %>%
  select_var() %>%
  mutate(method = sprintf("%s_nn", method))
tp_lic_final_nn <- merge(readRDS("results_simul/compile_tp/troughs_localic_final_nn.RDS"),
                         readRDS("results_simul/compile_tp/peaks_localic_final_nn.RDS"),
                         by=c("series", "kernel", "h", "degree", "method"))  %>%
  dplyr::filter(degree == "d2", h == "h6") %>%
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>%
  select(!c(degree, h)) %>%
  mutate(method = sprintf("%s_nn", method))

tp_arima <-
  merge(readRDS("results_simul/compile_tp/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp/peaks_arima.RDS"),
        by=c("series","kernel", "method", "ny")) %>%
  select_var() %>%
  mutate(method = ifelse(ny == "All", method, paste0(method,"_ny",ny))) %>%
  mutate(ny = NULL)


order_methods  <- c(
  "lc", "lc_nn",
  "lc_localic_final_d2_h3", "lc_localic_final_d2_h4", "lc_localic_final_d2_h5", "lc_localic_final_d2_h6",
  "lc_localic_final_d3_h3", "lc_localic_final_d3_h4", "lc_localic_final_d3_h5",
  "lc_localic_final_d3_h6","lc_localic_final_nn",
  "lc_localic_d2_h3", "lc_localic_d2_h4", "lc_localic_d2_h5",
  "lc_localic_d2_h6", "lc_localic_d3_h3", "lc_localic_d3_h4", "lc_localic_d3_h5",
  "lc_localic_d3_h6",
  "ql", "ql_nn",
  "ql_localic_final_d2_h3", "ql_localic_final_d2_h4", "ql_localic_final_d2_h5", "ql_localic_final_d2_h6",
  "ql_localic_final_d3_h3", "ql_localic_final_d3_h4", "ql_localic_final_d3_h5",
  "ql_localic_final_d3_h6",
  "ql_localic_final_nn",
  "ql_localic_d2_h3", "ql_localic_d2_h4", "ql_localic_d2_h5",
  "ql_localic_d2_h6", "ql_localic_d3_h3", "ql_localic_d3_h4", "ql_localic_d3_h5",
  "ql_localic_d3_h6",
  "cq", "cq_nn",
  "daf", "daf_nn",
  "auto_arima_ny2", "auto_arima_ny4", "auto_arima_ny6",
  "auto_arima_ny8", "auto_arima_ny10", "auto_arima_ny12",
  "auto_arima_ny14", "auto_arima_ny16", "auto_arima_ny18",
  "auto_arima_ny20", "auto_arima_ny25", "auto_arima_ny30",
  "auto_arima")
all_tp <- rbind(
  tp_lp,
  tp_lic_final,
  tp_lic_daf_trunc,
  tp_arima,
  tp_lp_nn,
  tp_lic_final_nn) %>%
  mutate(method = factor(method, order_methods,
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

legende <- c(lc = "LC", ql = "QL",
             cq = "CQ", daf = "DAF",
             lc_localic_final_d2_h6 = "LC loc. param.\n(final estimates)",
             lc_localic_d2_h6 = "LC loc.\nparam.",
             ql_localic_final_d2_h6 = "QL loc. param.\n(final estimates)",
             ql_localic_d2_h6 = "QL loc.\nparam.",
             auto_arima_ny12 = "ARIMA")
p <- ggplot(all_tp %>%
              dplyr::filter(method %in%
                              names(legende),
                            kernel == "henderson")  %>%
              format_table_tp() ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p

ggsave("paper/img/simulations/phase_shift_simul_rev.pdf",
       plot = p,
       width = 10, height = 6)
