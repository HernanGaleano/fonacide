options(viewer = function(url, height = NULL)
{
  if (!is.character(url) || (length(url) != 1))
    stop("url must be a single element character vector.", call. = FALSE)
  
  if (identical(height, "maximize"))
    height <- -1
  
  if (!is.null(height) && (!is.numeric(height) || (length(height) != 1)))
    stop("height must be a single element numeric vector or 'maximize'.", call. = FALSE)
  
  invisible(.Call("rs_showPageViewer", url, title = "RStudio", self_contained = FALSE))  
})