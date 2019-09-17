#' huxtable_to_png
#' @param tab Huxtable
#' @param file Resultant .png file
#' @export
huxtable_to_png <- function(tab, file) {
  name <- stringr::str_remove(file, ".png")
  dir <- fs::path_dir(file)

  name_tex <- paste(name, ".tex", sep = "")

  output <- glue::glue("
\documentclass{{standalone}}
\usepackage{{booktabs}}
\usepackage{{array}}
\usepackage{{caption}}
\usepackage{{graphicx}}
\usepackage{{siunitx}}
\usepackage{{colortbl}}
\usepackage{{multirow}}
\usepackage{{hhline}}
\usepackage{{calc}}
\usepackage{{tabularx}}
\usepackage{{threeparttable}}
\usepackage{{wrapfig}}

\begin{{document}}
{huxtable::to_latex(tab,tabular_only=T)}
\end{{document}}")
  cat(output, file = name_tex)

  withr::with_dir(dir, tools::texi2dvi(file = name_tex))

  cmd <- paste("cd", shQuote(dir), "; dvipng -T tight -D 1200 -z 9", shQuote(paste(name, ".dvi", sep = "")))
  invisible(system(cmd))

  cleaner <- c(".tex", ".aux", ".log", ".dvi")
  invisible(file.remove(paste(name, cleaner, sep = "")))

  old <- paste0(name, "1.png")
  new <- paste0(name, ".png")
  fs::file_move(old, new)

  return(invisible(file))
}
