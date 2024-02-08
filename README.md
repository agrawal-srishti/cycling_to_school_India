# Cycling to school levels code

This repository includes the code developed to analyse levels of cycling to school in India using three rounds of a population-representative education survey. It contains R scripts used in pre-processing data, harmonising data across three rounds and analysis (tables and graphs) for our manuscript, “A silent revolution: Rapid rise of cycling to school in rural India”.

## Directories

- `bicycle_distribution_schemes/` includes data object for bicycle distribution schemes across states, starting year of the scheme, and its applicability (rural/urban x boys/girls).
- `code/` includes two R scripts, one for harmonising data across three survey rounds, and the other for analysis.
- `lookup_tables/` includes district and state lookup files.

## R version and packages

We used the R version 4.2.1 and installed the following packages: 
- "dplyr", "tidyr", “stringr”, “tidyverse”
- For making graphs: “ggplot2”, “ggrepel”, “ggpubr”
- Survey data analysis: “survey”, “haven”, “jtools”, “remotes”, “svrepmisc”
- Generalised additive models (GAM) plots: “mgcv”, “tidymv”
- Please run the following line to install the required packages:  `install.packages(c("dplyr", "tidyr", "stringr", "tidyverse", "ggplot2", "ggrepel", "ggpubr", "survey", "haven", "jtools", "remotes", "svrepmisc", "mgcv","tidymv"))`

## Workflow

### Data pre-processing

- Please download the data using the links to the survey provided in the references section of the manuscript.
- Run the `code/cycling_to_school_harmonise_data.R` script to extract variables from the three survey rounds and create a combined database. It uses the district and state lookup tables available in the directory, `lookup_tables/`.

### Data analysis

- Run the `code/cycling_to_school_data_analysis.R` script to find levels of cycling to school using the “survey” package. It uses the information on bicycle distribution schemes available in the directory,`bicycle_distribution_schemes/` to find the relationship between cycling-to-school levels and BDS implementation across states.

## Contributors and contact

- Contributors: Srishti Agrawal, Adit Seth and Rahul Goel
- Maintainer contact: Srishti Agrawal (agrawalsrishti10@gmail.com)

## R package citations

- R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
- Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. ISBN 978-3-319-24277-4, https://ggplot2.tidyverse.org](https://ggplot2.tidyverse.org/.
- Wood S (2017). *Generalized Additive Models: An Introduction with R*, 2 edition. Chapman and Hall/CRC. https://cran.r-project.org/web/packages/mgcv/index.html
- Lumley T (2023). “survey: analysis of complex survey samples.” R package version 4.2. https://cran.r-project.org/web/packages/survey/index.html
