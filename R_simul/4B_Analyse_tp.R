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
  dplyr::filter(degree == "d2", h == "h6") %>%
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>%
  select(!c(degree, h))
tp_lic_daf_trunc <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_daf_trunc.RDS"),
                              readRDS("results_simul/compile_tp_norev/peaks_localic_daf_trunc.RDS"),
                              by=c("series", "kernel", "h", "degree", "method")) %>%
  dplyr::filter(degree == "d2", h == "h6") %>%
  select_var() %>% mutate(method = sprintf("%s_localic", method)) %>%
  select(!c(degree, h))

tp_arima <-
  merge(readRDS("results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
fst_weights <- c("weight235")

all_tp <- rbind(tp_lp,
                tp_lic_final,
                tp_lic_daf_trunc,
                tp_arima) %>%
  mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                  "ql", "ql_localic_final", "ql_localic",
                                  "cq","daf", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))


# Graphique sur le dephasage
format_table_tp <- function(x){
  x %>%
    tidyr::pivot_longer(
      cols = starts_with("x"),
      names_to = "name",
      values_to = "value"
    )%>% dplyr::filter(kernel == "henderson") %>%
    unique_series_pivot() %>%
    mutate(variability = recode(variability,
                                lowvariability = "Low variability",
                                mediumvariability = "Medium  variability",
                                highvariability = "High variability")) %>%
    na.omit()
}
data_tp <- all_tp %>% format_table_tp()


legende <- c(lc = "LC", ql = "QL",
             cq = "CQ", daf = "DAF",
             lc_localic_final = "LC loc. param.\n(final estimates)",
             lc_localic = "LC loc.\nparam.",
             ql_localic_final = "QL loc. param.\n(final estimates)",
             ql_localic = "QL loc.\nparam.",
             auto_arima = "ARIMA")
p <- ggplot(data_tp ,aes(x=method, y = value))+
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Phase shift", x = NULL) +
  scale_x_discrete(labels = legende) +
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p


ggsave("img/simulations/phase_shift_simul.pdf",
            plot = p,
            width = 10, height = 6)
