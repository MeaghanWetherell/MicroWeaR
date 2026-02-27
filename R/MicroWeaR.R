#' @name MicroWeaR-package
#' @aliases MicroWeaR
#' @title MicroWeaR is a Package to Examine and Score Microwear Scars
#' @author Antonio Profico, Flavia Strani, Pasquale Raia, Daniel DeMiguel
#' @description A tool that provides a summary statistic of the sampled scars for the user. The package includes functions to sample the scars, classify features as pits or scratches and then into consider also their respective subcategories, generate an output table with summary information, and obtain a visual surface-map in where scars are tracked.
#' @import jpeg
#' @import png
#' @import tiff
#' @import RANN
#' @import zoom
#' @import bmp
#' @importFrom grDevices dev.cur dev.next dev.off dev.prev dev.set graphics.off x11
#' @importFrom graphics locator plot points rasterImage rect text
#' @importFrom stats sd
#' @importFrom utils combn
#' @importFrom dplyr mutate filter left_join %>%
"_PACKAGE"
NULL
