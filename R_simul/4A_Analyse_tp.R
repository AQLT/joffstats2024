# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(latex2exp)

tp_lp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
               readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
               by=c("series","kernel", "method")) %>%
  select_var()

tp_lic_final <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_final.RDS"),
                      readRDS("results_simul/compile_tp_norev/peaks_localic_final.RDS"),
                      by=c("series", "kernel", "h", "degree", "method"))  %>%
  # dplyr::filter() %>%
  # mutate(method = ifelse(degree == "d2", h == "h6"))
  select_var() %>%
  mutate(method = sprintf("%s_localic_final_%s_%s", method, degree, h)) %>%
  select(!c(degree, h))

tp_lic_daf_trunc <- merge(
  readRDS("results_simul/compile_tp_norev/troughs_localic_daf_trunc.RDS"),
  readRDS("results_simul/compile_tp_norev/peaks_localic_daf_trunc.RDS"),
  by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var() %>%
  mutate(method = sprintf("%s_localic_%s_%s", method, degree, h)) %>%
  select(!c(degree, h))


tp_lp_nn <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp_nn.RDS"),
                  readRDS("results_simul/compile_tp_norev/peaks_lp_nn.RDS"),
                  by=c("series","kernel", "method")) %>%
  select_var() %>%
  mutate(method = sprintf("%s_nn", method))
tp_lic_final_nn <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_final_nn.RDS"),
                         readRDS("results_simul/compile_tp_norev/peaks_localic_final_nn.RDS"),
                         by=c("series", "kernel", "h", "degree", "method"))  %>%
  dplyr::filter(degree == "d2", h == "h6") %>%
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>%
  select(!c(degree, h)) %>%
  mutate(method = sprintf("%s_nn", method))

tp_arima <-
  merge(readRDS("results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method", "ny")) %>%
  select_var() %>%
  mutate(method = ifelse(ny == "All", method, paste0(method,"_ny",ny))) %>%
  mutate(ny = NULL)
tp_ner_neigh_hend <-
  merge(readRDS("results_simul/compile_tp_norev/troughs_ner_neigh_hend.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_ner_neigh_hend.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()


order_methods  <- c(
  "lc", 
  "lc_localic_final_d2_h3", "lc_localic_final_d2_h4", "lc_localic_final_d2_h5", "lc_localic_final_d2_h6",
  "lc_localic_final_d3_h3", "lc_localic_final_d3_h4", "lc_localic_final_d3_h5",
  "lc_localic_final_d3_h6",
  "lc_localic_d2_h3", "lc_localic_d2_h4", "lc_localic_d2_h5",
  "lc_localic_d2_h6", "lc_localic_d3_h3", "lc_localic_d3_h4", "lc_localic_d3_h5",
  "lc_localic_d3_h6","lc_nn", "lc_localic_final_nn",
  "ql", 
  "ql_localic_final_d2_h3", "ql_localic_final_d2_h4", "ql_localic_final_d2_h5", "ql_localic_final_d2_h6",
  "ql_localic_final_d3_h3", "ql_localic_final_d3_h4", "ql_localic_final_d3_h5",
  "ql_localic_final_d3_h6",
  "ql_localic_d2_h3", "ql_localic_d2_h4", "ql_localic_d2_h5",
  "ql_localic_d2_h6", "ql_localic_d3_h3", "ql_localic_d3_h4", "ql_localic_d3_h5",
  "ql_localic_d3_h6","ql_nn","ql_localic_final_nn",
  "cq", "cq_nn",
  "daf", "daf_nn",
  "auto_arima_ny2", "auto_arima_ny4", "auto_arima_ny6",
  "auto_arima_ny8", "auto_arima_ny10", "auto_arima_ny12",
  "auto_arima_ny14", "auto_arima_ny16", "auto_arima_ny18",
  "auto_arima_ny20", "auto_arima_ny25", "auto_arima_ny30",
  "auto_arima",
  "nearest_neighbour_henderson")
all_tp <- rbind(
  tp_lp,
  tp_lic_final,
  tp_lic_daf_trunc,
  tp_arima,
  tp_ner_neigh_hend,
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

ggsave("paper/img/simulations/phase_shift_simul.pdf",
       plot = p,
       width = 10, height = 6)
legende <- c(
  lc = "LC", lc_nn = "LC\n(nearest neighbors)",
  # lc_localic_final = "LC loc. param.\n(final estimates)", lc_localic_final_nn = "LC loc. param.\n(final estimates) NN",
  # lc_localic = "LC loc.\nparam.", lc_localic_nn = "LC loc.\nparam. NN",
  ql = "QL", ql_nn = "QL\n(nearest neighbors)",
  # ql_localic_final = "QL loc. param.\n(final estimates)", ql_localic_final_nn = "QL loc. param.\n(final estimates) NN",
  # ql_localic = "QL loc.\nparam.", ql_localic_nn = "QL loc.\nparam. NN",
  cq = "CQ", cq_nn = "CQ\n(nearest neighbors)",
  daf = "DAF", daf_nn = "DAF\n(nearest neighbors)"
)

p_nn <- ggplot(all_tp  %>%
                 dplyr::filter(method %in%
                                 names(legende),
                               kernel == "henderson") %>%
                 format_table_tp(),aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende)
p_nn

ggsave("paper/img/simulations/phase_shift_simul_nn_lp.pdf",
       plot = p_nn,
       width = 10, height = 6)

legende <- c(
  # lc = "LC", lc_nn = "LC NN",
  lc_localic_d2_h6 = "LC loc.\nparam.", lc_localic_final_nn = "LC loc. param.\n(final estimates)\n(nearest neighbors)",
  # ql = "QL", ql_nn = "QL NN",
  ql_localic_d2_h6 = "QL loc.\nparam.", ql_localic_final_nn = "QL loc. param.\n(final estimates)\n(nearest neighbors)"
  # cq = "CQ", cq_nn = "CQ NN",
  # daf = "DAF", daf_nn = "DAF NN"
)

p_local_nn <- ggplot(all_tp  %>%
                       dplyr::filter(method %in%
                                       names(legende),
                                     kernel == "henderson") %>%
                       format_table_tp() ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende)
p_local_nn

ggsave("paper/img/simulations/phase_shift_simul_nn_lp_localparam.pdf",
       plot = p_local_nn,
       width = 10, height = 6)

legende <-
  c(
    "auto_arima_ny2" = "2 years",
    "auto_arima_ny4" = "4 years",
    "auto_arima_ny6" = "6 years",
    "auto_arima_ny8" = "8 years",
    "auto_arima_ny10" = "10 years",
    "auto_arima_ny12" = "12 years",
    "auto_arima_ny14" = "14 years",
    "auto_arima_ny16" = "16 years",
    "auto_arima_ny18" = "18 years",
    "auto_arima_ny20" = "20 years",
    "auto_arima_ny25" = "25 years",
    "auto_arima_ny30" = "30 years",
    "auto_arima" = "Full span"
  )
p_arima <- ggplot(all_tp  %>%
                    dplyr::filter(method %in%
                                    names(legende),
                                  kernel == "henderson") %>%
                    format_table_tp(),
                  aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende)
p_arima
ggsave("paper/img/simulations/phase_shift_simul_arima_length.pdf",
       plot = p_arima,
       width = 10, height = 6)


legende <- c(
  "lc_localic_final_d2_h3" = "d=2, h=3",
  "lc_localic_final_d2_h4" = "d=2, h=4",
  "lc_localic_final_d2_h5" = "d=2, h=5",
  "lc_localic_final_d2_h6" = "d=2, h=6",
  "lc_localic_final_d3_h3" = "d=3, h=3",
  "lc_localic_final_d3_h4" = "d=3, h=4",
  "lc_localic_final_d3_h5" = "d=3, h=5",
  "lc_localic_final_d3_h6" = "d=3, h=6")
p_lc_final <- ggplot(all_tp %>%
                 dplyr::filter(method %in%
                                 names(legende),
                               kernel == "henderson")  %>%
                 format_table_tp() ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p_lc_final

legende <- c(
  "lc_localic_d2_h3" = "d=2, h=3",
  "lc_localic_d2_h4" = "d=2, h=4",
  "lc_localic_d2_h5" = "d=2, h=5",
  "lc_localic_d2_h6" = "d=2, h=6",
  "lc_localic_d3_h3" = "d=3, h=3",
  "lc_localic_d3_h4" = "d=3, h=4",
  "lc_localic_d3_h5" = "d=3, h=5",
  "lc_localic_d3_h6" = "d=3, h=6")
p_lc <- ggplot(all_tp %>%
                       dplyr::filter(method %in%
                                       names(legende),
                                     kernel == "henderson")  %>%
                       format_table_tp() ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p_lc

legende <- c(
  "ql_localic_final_d2_h3" = "d=2, h=3",
  "ql_localic_final_d2_h4" = "d=2, h=4",
  "ql_localic_final_d2_h5" = "d=2, h=5",
  "ql_localic_final_d2_h6" = "d=2, h=6",
  "ql_localic_final_d3_h3" = "d=3, h=3",
  "ql_localic_final_d3_h4" = "d=3, h=4",
  "ql_localic_final_d3_h5" = "d=3, h=5",
  "ql_localic_final_d3_h6" = "d=3, h=6")
p_ql_final <- ggplot(all_tp %>%
                 dplyr::filter(method %in%
                                 names(legende),
                               kernel == "henderson")  %>%
                 format_table_tp() ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p_ql_final


legende <- c(
  "ql_localic_d2_h3" = "d=2, h=3",
  "ql_localic_d2_h4" = "d=2, h=4",
  "ql_localic_d2_h5" = "d=2, h=5",
  "ql_localic_d2_h6" = "d=2, h=6",
  "ql_localic_d3_h3" = "d=3, h=3",
  "ql_localic_d3_h4" = "d=3, h=4",
  "ql_localic_d3_h5" = "d=3, h=5",
  "ql_localic_d3_h6" = "d=3, h=6")
p_ql <- ggplot(all_tp %>%
                       dplyr::filter(method %in%
                                       names(legende),
                                     kernel == "henderson")  %>%
                       format_table_tp(),aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p_ql

ggsave("paper/img/simulations/phase_shift_simul_local_lc.pdf",
       plot = p_lc,
       width = 10, height = 6)
ggsave("paper/img/simulations/phase_shift_simul_local_final_lc.pdf",
       plot = p_lc_final,
       width = 10, height = 6)

ggsave("paper/img/simulations/phase_shift_simul_local_ql.pdf",
       plot = p_ql,
       width = 10, height = 6)
ggsave("paper/img/simulations/phase_shift_simul_local_final_ql.pdf",
       plot = p_ql_final,
       width = 10, height = 6)


kernels <- c("henderson", "biweight", "gaussian", "parabolic", "triangular",
             "tricube", "triweight", "uniform")
data_tp_kernel <- tp_lp %>%
  mutate(kernel = factor(kernel, kernels,
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE)) %>%
  format_table_tp(kernel = kernels)
p_kernel <- ggplot(data_tp_kernel %>% dplyr::filter(method == "lc"),
                   aes(x=kernel, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL)
p_kernel

ggsave("paper/img/simulations/phase_shift_kernel.pdf",
       plot = p_kernel,
       width = 10, height = 6)

