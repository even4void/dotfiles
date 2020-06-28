options(menu.graphics = FALSE)
options(width = 100, tibble.width = 100)
makeActiveBinding(".__", function() .Last.value, .GlobalEnv)

## alias
.env <- new.env()
assign("su", base::summary, env = .env)
assign("last", function(x) tail(x, n = 1), env = .env)
assign("ht", function(d) rbind(head(d, 10), tail(d, 10)), env = .env)
assign("h5", function(d) d[1:5, 1:5], env = .env)
assign("pp", function(d) tibble::trunc_mat(d), env = .env)
attach(.env)

## Maybe replace pp with:
## setMethod("show", "data.frame", function(object) tibble::trunc_mat(object))

## console (not Emacs)
if (Sys.getenv("TERM")  == "xterm-256color") {
  library("colorout")
  .bw_col <- 15
  setOutputColors(normal = .bw_col, negnu = .bw_col, zero = .bw_col,
                  number = .bw_col, date = .bw_col, string = .bw_col,
                  const = .bw_col, false = .bw_col, true = .bw_col,
                  infinite = .bw_col, index = 59, stderror = 136,
                  error = 166, warn = 136, verbose = FALSE)
}

## autoload
.loader <- function(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
.pkg <- c("ggplot2", "skimr")
if (interactive()) invisible(sapply(.pkg, .loader))

.First <- function() {
  grDevices::quartz.options(width = 6, height = 6)
  grDevices::palette("Tableau10")
}

.Last <- function() {
  if (interactive()) {
    hf <- Sys.getenv("R_HISTFILE")
    if (hf == "") hf <- "~/.RHistory"
    savehistory(hf)
  }
}
