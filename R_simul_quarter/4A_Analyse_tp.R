# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul_quarter/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(latex2exp)

tp_lp <- merge(readRDS("results_simul_quarter/compile_tp_norev/troughs_lp.RDS"),
                   readRDS("results_simul_quarter/compile_tp_norev/peaks_lp.RDS"),
                   by=c("series","kernel", "method")) %>%
  select_var()

tp_lic_final <- merge(readRDS("results_simul_quarter/compile_tp_norev/troughs_localic_final.RDS"),
                          readRDS("results_simul_quarter/compile_tp_norev/peaks_localic_final.RDS"),
                          by=c("series", "kernel", "h", "degree", "method"))  %>%
  dplyr::filter(degree == "d2", h == "h2") %>%
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>%
  select(!c(degree, h))
tp_lic_daf_trunc <- merge(readRDS("results_simul_quarter/compile_tp_norev/troughs_localic_daf_trunc.RDS"),
                              readRDS("results_simul_quarter/compile_tp_norev/peaks_localic_daf_trunc.RDS"),
                              by=c("series", "kernel", "h", "degree", "method")) %>%
  dplyr::filter(degree == "d2", h == "h2") %>%
  select_var() %>% mutate(method = sprintf("%s_localic", method)) %>%
  select(!c(degree, h))

tp_arima <-
  merge(readRDS("results_simul_quarter/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_simul_quarter/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method", "ny")) %>%
  select_var() %>% 
  mutate(method = ifelse(ny == "All", method, paste0(method,"_ny",ny))) %>% 
  mutate(ny = NULL)
# tp_ner_neigh <-
#   merge(readRDS("results_simul_quarter/compile_tp_norev/troughs_ner_neigh.RDS"),
#         readRDS("results_simul_quarter/compile_tp_norev/peaks_ner_neigh.RDS"),
#         by=c("series","kernel", "method")) %>%
#   select_var()

order_methods  <- c("lc", "lc_localic_final", "lc_localic",
  "ql", "ql_localic_final", "ql_localic",
  "cq","daf", 
  "auto_arima_ny2", "auto_arima_ny4", "auto_arima_ny6",
  "auto_arima_ny8", "auto_arima_ny10", "auto_arima_ny12", 
  "auto_arima_ny14", "auto_arima_ny16", "auto_arima_ny18",
  "auto_arima_ny20", "auto_arima_ny25", "auto_arima_ny30",
  "auto_arima",
  "nearest_neighbour")
all_tp <- rbind(tp_lp,
                tp_lic_final,
                tp_lic_daf_trunc,
                tp_arima) %>%
  mutate(method = factor(method, order_methods,
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

data_tp <- all_tp %>% format_table_tp()


legende <- c(lc = "LC", ql = "QL",
             cq = "CQ", daf = "DAF",
             lc_localic_final = "LC loc. param.\n(final estimates)",
             lc_localic = "LC loc.\nparam.",
             ql_localic_final = "QL loc. param.\n(final estimates)",
             ql_localic = "QL loc.\nparam.",
             auto_arima = "ARIMA")
p <- ggplot(data_tp %>% 
              filter(method %in%
                       names(legende)) ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p


ggsave("paper/img/simulations/phase_shift_simul.pdf",
            plot = p,
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
p <- ggplot(data_tp %>% 
              filter(method %in%
                       names(legende)),
                     aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) 
p

kernels <- c("henderson", "biweight", "gaussian", "parabolic", "triangular", 
            "tricube", "triweight", "uniform")


data_tp_kernel <- tp_lp %>%
  mutate(kernel = factor(kernel, kernels,
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE)) %>% 
  format_table_tp(kernel = kernels)
p_kernel <- ggplot(data_tp_kernel %>% filter(method == "lc"),
            aes(x=kernel, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL)
p_kernel

all.equal(tp_lp %>% filter(kernel == "henderson") %>% select(!kernel),
          tp_lp %>% filter(kernel == "biweight")%>% select(!kernel))
