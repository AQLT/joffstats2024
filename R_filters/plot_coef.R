library(rjd3filters)
library(patchwork)
library(ggdemetra3)
library(ggplot2)

all_filters <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
  lp_filter(
    horizon = 6,
    degree = 3,
    kernel = "Henderson",
    endpoints = endpoints,
    ic = 3.5,
    tweight = 0,
    passband = pi/12
  )
})


names(all_filters) <- c("LC", "QL", "CQ", "DAF")


for(i in names(all_filters)){
  print(i)
  x <- all_filters[[i]]
  p <- ggplot_coef(x, q = c(0:6)) / (
    ggplot_gain(x, q = c(0:6)) +
      ggplot2::scale_y_continuous(
        "Gain",
        breaks = seq(0, 1, by = 0.2))+
      ggplot2::guides(colour = "none") +
      ggplot_phase(x, xlim = c(0, 4/12*pi), q = c(0:6))+
      ggplot2::labs(y = "Phase shift") +
      ggplot2::guides(colour = "none"))
  p <- p & ggplot2::scale_color_grey()
  ggsave(sprintf("paper/img/filters_used/%s.pdf",tolower(i)),
              width = 8, height = 4.5,
              plot = p)
}

x <- all_filters[["LC"]]
p <- (ggplot_coef(x, q=0) + ggplot2::guides(colour = "none") )/ (
  ggplot_gain(x, q = 0)  +
    ggplot2::scale_y_continuous(
      "Gain",
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, NA))+ ggplot2::guides(colour = "none") +
    ggplot_phase(x, xlim = c(0, pi), q = 0)+
    ggplot2::labs(y = "Phase shift") + ggplot2::guides(colour = "none")
)
p <- p & scale_color_grey()
p
ggsave("paper/img/filters_used/musgrave.pdf",
       width = 8, height = 4.5,
       plot = p)


h_filter <- lp_filter(horizon = 6)@sfilter
nn_ma <- list(
  henderson = finite_filters(h_filter, lapply(5:0, function(i){
    fst_filter(lags = 13-i-1, leads = i)
  })),
  `d=2` = finite_filters(h_filter, lapply(1:6, function(i){
    lp_filter(horizon=6+i, endpoints = "DAF", degree = 2)[,2*i+1]
  })),
  `d=3` = finite_filters(h_filter, lapply(1:6, function(i){
    lp_filter(horizon=6+i, endpoints = "DAF", degree = 3)[,2*i+1]
  }))
)
x <- nn_ma[["henderson"]]
p <- ggplot_coef(x, q = c(0:6)) / (
  ggplot_gain(x, q = c(0:6)) +
    ggplot2::scale_y_continuous(
      "Gain",
      breaks = seq(0, 1, by = 0.2))+
    ggplot2::guides(colour = "none") +
    ggplot_phase(x, xlim = c(0, 4/12*pi), q = c(0:6))+
    ggplot2::labs(y = "Phase shift") +
    ggplot2::guides(colour = "none"))
p <- p & ggplot2::scale_color_grey()
p
ggsave("paper/img/filters_used/nn_henderson.pdf",
       width = 8, height = 4.5,
       plot = p)

h_filter <- lp_filter(horizon = 6, kernel = "Henderson")@sfilter
all_filters <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
  finite_filters(h_filter, lapply(1:6, function(i){
    lp_filter(horizon=6+i, endpoints = endpoints,
              kernel = "Henderson",
              ic = 3.5)[,2*i+1]
  }))
})
names(all_filters) <- c("LC", "QL", "CQ", "DAF")
for(i in names(all_filters)){
  print(i)
  x <- all_filters[[i]]
  p <- ggplot_coef(x, q = c(0:6)) / (
    ggplot_gain(x, q = c(0:6)) +
      ggplot2::scale_y_continuous(
        "Gain",
        breaks = seq(0, 1, by = 0.2))+
      ggplot2::guides(colour = "none") +
      ggplot_phase(x, xlim = c(0, 4/12*pi), q = c(0:6))+
      ggplot2::labs(y = "Phase shift") +
      ggplot2::guides(colour = "none"))
  p <- p & ggplot2::scale_color_grey()
  ggsave(sprintf("paper/img/filters_used/%s_nn.pdf",tolower(i)),
         width = 8, height = 4.5,
         plot = p)
}
