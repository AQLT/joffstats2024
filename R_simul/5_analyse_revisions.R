# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_methods <- c("lp", "arima", "localic_daf_trunc", "localic_final")

for (crit in c("ce", "fe")) {
  for (method in all_methods) {
    data <- readRDS(sprintf("results_simul/compile_revisions/%s_%s_rev.RDS", method, crit)) %>%
      select_series() %>%
      select_mae()
    if (method == "lp")
      data <- data %>% dplyr::filter(kernel == "henderson")
    if (method %in% c("localic_daf_trunc", "localic_final")){
      suff <- ifelse(method == "localic_final", "_final", "")
      data <- data  %>% dplyr::filter(degree == "d2", h == "h6") %>%
        mutate(method = sprintf("%s_localic%s", method,
                                suff)) %>%
        select(!c(degree, h))
    }
    data <- data %>% select(!kernel)
    assign(sprintf("rev_%s_%s", crit, method), data)
  }
  all_data <- do.call(rbind, mget(sprintf("rev_%s_%s", crit, all_methods))) %>%
    mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                    "ql", "ql_localic_final", "ql_localic",
                                    "cq","daf", "auto_arima"),
                           ordered = TRUE),
           variability = factor(variability,
                                levels = c("lowvariability","mediumvariability","highvariability"),
                                ordered = TRUE))
  assign(sprintf("all_rev_%s", crit), all_data)
}

x = all_rev_fe
normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  ref = x[(x$method == "lc"),grep(suff,colnames(x)) ]
  for(m in unique(x$method)){
    if(nrow(x[x$method == m,grep(suff,colnames(x))]) > 0){
      x[x$method == m,grep(suff,colnames(x))] <-
        x[x$method == m,grep(suff,colnames(x))] / ref
    }
  }
  x
}
summarise_ref <- function(x, normalise = FALSE){
  if(normalise){
    x = x %>% normalise_rev()
    digits = 1
  } else{
    digits = 2
  }
  x %>%
    group_by(variability, method) %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = list(Mean = \(x) round(mean(x),digits)),
      .names = "{col}"
    )) %>%
    select(!c(rev.q6:rev.q10, length)) %>%
    data.frame()
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref())
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE))
rev_tot %>% dplyr::filter(variability == "mediumvariability")
rev_rel %>% dplyr::filter(variability == "mediumvariability")
rev_tot %>% dplyr::filter(method %in% c("lc", "gain"))

rev_table <- rev_tot %>% dplyr::filter(variability == "mediumvariability")%>%
  select(!c(variability)) %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         lc_localic_final = "LC local param. (final estimates)",
                         lc_localic = "LC local param.",
                         ql_localic_final = "QL local param. (final estimates)",
                         ql_localic = "QL local param.",
                         auto_arima = "ARIMA")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
saveRDS(rev_table, file = "data/simulations_revisions.RDS")
