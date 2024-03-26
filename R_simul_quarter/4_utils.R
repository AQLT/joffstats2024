library(dplyr)
library(AQLThesis)

length_info <- 
  sapply(list.files(path = "data_simul_quarter/byseriesinfo",full.names = TRUE), 
         function(f){
           info <- readRDS(f)
           5
         })
length_info <- data.frame(series = gsub(".RDS", "", basename(names(length_info))),
                          length = as.numeric(length_info))

select_var <- function(x){
  x = select_series(x)
  x
}
select_series <- function(x){
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE, by = "series")
  x$variability <- gsub("\\d","", x$series)
  x
}
select_mae <- function(x){
  x %>% 
    dplyr::filter(Group == "total",
           stats == "MAE") %>% 
    select(!c(Group, stats))
}


unique_series_pivot <- function(x){
  to_remove = x %>% group_by(series, name) %>%
    mutate(remove = any(is.na(value))) %>% 
    data.frame()
  x[to_remove$remove,"value"] <- NA
  x
}

# Graphique sur le dephasage
format_table_tp <- function(x, kernel = "henderson"){
  x %>%
    tidyr::pivot_longer(
      cols = starts_with("x"),
      names_to = "name",
      values_to = "value"
    )%>% dplyr::filter(kernel %in% kernel) %>%
    unique_series_pivot() %>%
    mutate(variability = recode(variability,
                                lowvariability = "Low variability",
                                mediumvariability = "Medium  variability",
                                highvariability = "High variability")) %>%
    na.omit()
}


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
    select(!c(rev.q3:rev.q10, length)) %>%
    data.frame()
}
