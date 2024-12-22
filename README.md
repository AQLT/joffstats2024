

# joffstats2024

This repository contains the code and data used in the paper:

Quartier-la-Tente, A. (2024). Improving Real-Time Trend Estimates Using Local Parametrization of Polynomial Regression Filters. *Journal of Official Statistics, 40*(4), 685-715. <https://doi.org/10.1177/0282423X241283207>

To cite this article:

```
@article{jos2024AQLT,
author={Quartier{-la-}Tente, Alain},
doi = {10.1177/0282423X241283207},
title = {Improving real-time trend estimates using local parametrisation of polynomial regression filters},
journal = {Journal of Official Statistics},
number = {4},
volume = {40},
year = {2024},
pages = {685-715}
}

```

## Installation

All programs are in R and mainly used [`rjd3filters`](https://github.com/rjdverse/rjd3filters) which requires a version of Java greater than or equal to 17.
To check the Java version used by R, use the command:

```{r, eval=FALSE}
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
```

If you do not have a compatible version, you can for example install a portable version from the following links:

- [Zulu JDK](https://www.azul.com/downloads/#zulu)

- [AdoptOpenJDK](https://adoptopenjdk.net/)

- [Amazon Corretto](https://aws.amazon.com/corretto/)

To install all the packages you need to have the `renv` package installed and once the project is loaded you just have to run the code `renv::restore()`.


## Description of the programs

All R programs are gathered in each folder, each containing an explanatory `README` file:

- `Simplified_example.R`: simplified example to apply the different filters described in the paper;

- `R_filters`: programs to plot the different moving averages used in the paper ;

- `R_fredm`: programs used to compare the filters on real data;

- `R_simul`: programs used to compare the filters on simulated monthly series;

- `R_simul_quarter`: programs used to compare the filters on simulated quarterly series.


# License

 [CC BY-NC 4.0 <img src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" alt="Licence Creative Commons" style="display: inline-block; margin: 0"/>](https://creativecommons.org/licenses/by-nc/4.0/).

