# MicroWeaR
MicroWeaR package provides the user with a summary statistic of the sampled scars that can be exported in different formats. 
MicroWeaR includes functions to sample the scars, classify features as pits or scratches and then into consider also their respective subcategories, generate an output table with summary information, and obtain a visual surface-map in where scars are tracked. 
To install this particular development version (beta) of *MicroWeaR* R-package from Github using *devtools*:

```{r} 
install.packages("devtools")
devtools::install_github("MeaghanWetherell/MicroWeaR/")
```

If running the previous lines the MicroWeaR installing fails, try this code:

```{r} 
install.packages("devtools")
library("devtools")
install.packages("zoom")
library(zoom)
install.packages("RANN")
library(RANN)
install_github("MeaghanWetherell/MicroWeaR/",local=FALSE)
library(MicroWeaR)
```

# Changelog

8.21.2026 - Version 1.1.1 now includes the feature density measurements in the results from outcome.Ico().
2.27.2026 - Created parr.plot() to include option to visualize categorization, fixed class.Ico() to accommodate different SEM image types, and other changes.


MicroWeaR mailing list: https://groups.google.com/forum/#!forum/microwear

Subscription: https://groups.google.com/forum/#!forum/microwear/join

