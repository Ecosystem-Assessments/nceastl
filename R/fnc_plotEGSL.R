#' Function to create base plots of the St. Lawrence and the data contained in the package
#'
#' @export
#'
#' @param layers character, vector of layers to plot on the graph, in plotting order.
#' @param prj character, projection to use to plot the graph.
#' @param extent numeric(c(xmn, ymn, xmx, ymx)) extent to plot in prj units OR  | character(data) to use extent of data (i.e. 'canada').
#' @param cols character, vector of fill colors for all layers specified in layers param, in same order.
#' @param borders character, vector of border colors for all layers specified in layers param, in same order.
#' @param lwds numeric, vector of border widths for all layers specified in layers param, in same order.
#' @param mar numeric(c(0,0,0,0)), plot margins.
#' @param box logical, draw box around graph.
#' @param axes numeric(c(1,2,3,4)), draw axes around graph, 1 = bottom; 2 = left; 3 = top; 4 = right.
#' @param background character, graph background color.
#' @param graticules character, graph graticules color.
#' @param scale, logical, draw a scale bar (NOT YET IMPLEMENTED)
#' @param northArrow, logical, draw a north arrow
#' @param prjText, logical, add projection name to map
#' @param prjCol, character, projection name font color
#'
#' @return Opens a graphical interface with the plot
#'
#' @keywords plot, st. lawrence
#'
#' @examples
#'# # Basic map
#'# plotEGSL()
#'#
#'# # Plot grid
#'# plotEGSL(grid = TRUE)
#'#
#'# # Quebec
#'# plotEGSL(layers = c('egslOutline','quebec','canada','usa'),
#'#          cols = c('#2196a8', '#25364A','#25364A','#C7CBCE'),
#'#          borders = '#f4f4f4',
#'#          northArrow = TRUE,
#'#          mar = c(2,2,2,2),
#'#          box = TRUE,
#'#          axes = 1:4,
#'#          extent = 'quebec')
#'#
#'# # Canada
#'# plotEGSL(layers = c('egslOutline','canada','usa'),
#'#         cols = c('#2196a8','#25364A','#C7CBCE'),
#'#         borders = '#f4f4f4',
#'#         northArrow = TRUE,
#'#         mar = c(2,2,2,2),
#'#         box = TRUE,
#'#         axes = 1:4,
#'#         extent = 'canada',
#'#         prj = slmetaPrj('world'))
#'#
#'# # Export
#'# jpeg('./essai.jpg', width = 5, height = 5, res = 300, units = 'in')
#'# plotEGSL(layers = 'egsl')
#'# dev.off()


# To do:
  # - cities/country names
  # - insert for larger view
  # - limits of estuary and gulf (see stlawrence.Rmd)
  # - add scale bar
  # - projection text on graph

  # - [] Add a default driver option in `plotEGSL()`
    # - type = character('default','driver','platform','etc')
  # - [] Add scale bar
  # - [] Add more detailed coastline, raster data not enough. I already have the data



plotEGSL <- function(layers     = 'egslOutline',
                     prj        = slmetaPrj('default'),
                     extent     = c(-340251.1, 162300.3, 1018039.9, 990801.3),
                     cols       = NULL,
                     borders    = NULL,
                     lwds       = NULL,
                     mar        = c(0,0,0,0),
                     box        = FALSE,
                     axes       = FALSE,
                     background = NULL,
                     graticules = NULL,
                     scale      = FALSE,
                     northArrow = FALSE,
                     prjText    = FALSE,
                     prjCol     = '#000000',
                     ...) {

  # Libraries
  library(graphicsutils)
  library(sf)

  ### ===   DATA & GRAPHICAL PARAMETERS   === ###


  # Layers
  # data(list = layers, envir = environment())

  # Projections
  for(i in layers) {
    if(st_crs(get(i))$proj4string != prj) {
      assign(i, st_transform(get(i, envir = globalenv()), crs = prj))
    }
  }

  # Extent: 'outline','quebec','canada','world'
  if (is.numeric(extent))    ext <- extent
  if (is.character(extent))  ext <- as.numeric(st_bbox(get(extent)))

  # Graphical parameters
  if (is.null(cols))    cols <- rep('#00000000', length(layers))
  if (is.null(borders)) borders <- rep('#25364A', length(layers))
  if (is.null(lwds))    lwds <- rep(1, length(layers))

  graph <- data.frame(cols             = cols,
                      borders          = borders,
                      lwds             = lwds,
                      row.names        = layers,
                      stringsAsFactors = F)



  ### ===   PLOT   === ###

  # Initiate plot
  par(mar = mar)
  graphicsutils::plot0(x = ext[c(1,3)],
        y = ext[c(2,4)],
        fill = background,
        grid.col = graticules)

  # Layers
  for(i in layers) {
    plot(st_geometry(get(i)),
         col = graph[i,'cols'],
         border = graph[i,'borders'],
         lwd = graph[i,'lwds'],
         add = T)
  }

  # North arrow
  if (northArrow) {
    xPos <- ext[1] + ((ext[3] - ext[1])*.95)
    yPos <- ext[2] + ((ext[4] - ext[2])*.95)
    graphicsutils::compassRose(xPos, yPos, cex.cr = .5)
  }

  # Box
  if (box) box2('1234', lwd = 3)

  # Axes
  if (!is.null(axes)) {
    if(1 %in% axes) axis(1, cex.axis = .8, lwd.ticks = 2, lwd = 2)
    if(2 %in% axes) axis(2, cex.axis = .8, lwd.ticks = 2, lwd = 2)
    if(3 %in% axes) axis(3, cex.axis = .8, lwd.ticks = 2, lwd = 2)
    if(4 %in% axes) axis(4, cex.axis = .8, lwd.ticks = 2, lwd = 2)
  }

  # Scale bar
  if (scale) {
    scale
  }

  # Projection text
  if (prjText) {
    text(x = ext[3],
         y = ext[2],
         labels = paste0("Projection: ", prj),
         adj = 1,
         cex = .4,
         col = prjCol)
  }
} # plotEGSL
