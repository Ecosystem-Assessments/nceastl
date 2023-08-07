#' Publication rendering
#' 
#' Function to render publications from this project
#' 
#' @describeIn render_pubs Render manuscript
#' @export
render_ms <- function(word = FALSE) {
  file.copy("./figures/", "./pubs/manuscript", recursive = TRUE)
  suppressWarnings({
    setwd('./pubs/manuscript/')
    rmarkdown::render('./FoodWeb-CumulativeImpact.md')

    if (word) {
      rmarkdown::render(
        './FoodWeb-CumulativeImpact.md', 
        output_format = "word_document"
      )      
    }
    setwd('../../')
  })
  unlink("./pubs/manuscript/figures/", recursive = TRUE)
}

#' @describeIn render_pubs Render manuscript supplementary materials
#' @export
render_ms_supmat <- function(word = FALSE) {
  file.copy("./figures/", "./pubs/manuscript", recursive = TRUE)
  suppressWarnings({
    setwd('./pubs/manuscript/')
    rmarkdown::render('./FoodWeb-CumulativeImpact-SI.md')

    if (word) {
      rmarkdown::render(
        './FoodWeb-CumulativeImpact-SI.md', 
        output_format = "word_document"
      )      
    }
    setwd('../../')
  })
  unlink("./pubs/manuscript/figures/", recursive = TRUE)
}

