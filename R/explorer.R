# function for getting names from data object
getDataList <- function(inputData){
  if (is.list(inputData)){
    if(is.null(names(inputData))){
      names_spec <- rep("", length(inputData))
    }else{
      names_spec <- names(inputData)
    }
    names_auto <- deparse(substitute(inputData))
    names_auto <- gsub("list\\(|\\)","", names_auto)
    names_auto <- unlist(lapply(strsplit(names_auto, split = ","), trimws))
    
    out <- data.frame(names_spec, names_auto, stringsAsFactors = FALSE)
    namesList <- ifelse(out$names_spec=="", out$names_auto, out$names_spec)
    out <- NULL
    names(inputData) <- NULL
  } else { 
    namesList <- inputData
  }
  
  if(is.list(inputData)){
    dataList <- inputData
  } else if (is.vector(inputData)){
    dataList <- sapply(inputData, get)
  } else { error('`data` is not a list or character vector')}
  
  names(dataList) <- namesList
  
  return(dataList)
}


# format data list
formatDataList <- function(dataList){
  dataList_formatted <- list()
  for (i in seq_along(dataList)){
    dataList_formatted[[i]] <- list(
      File = names(dataList)[i],
      Rows = nrow(dataList[[i]]),
      Columns = ncol(dataList[[i]]),
      json = jsonlite::toJSON(dataList[[i]])
    ) 
  }
  return(dataList_formatted)
}


#' Create an interactive codebook
#'
#' This function creates a series of interactive codebook using R htmlwidgets.
#'
#' @param data  A list of (optionally named) data frames. Also accepts a character vector of data frame names (must be loaded in environment).
#' @param addEnv  Logical. Indicates whether to add all data frames in current environemnt to explorer. Defaults to \code{addEnv=TRUE}.
#' @param demo   Logical. Indicates whether to display demo data frames.  If \code{TRUE}, \code{data} and \code{addEnv} settings will be ignored.  Defaults to \code{demo=FALSE}.

#'
#' @examples
#' explorer(data = list(Cars = mtcars, Iris = iris))
#' 
#' explorer(data = c("mtcars", "iris"))
#'
#' @import htmlwidgets
#' @importFrom jsonlite toJSON
#'
#' @export
explorer <- function(data = NULL, addEnv=TRUE, demo=FALSE) {
  
  # (1) Initialize the settings with the raw values passed to the r function
  rSettings = list(
    rParams=list(
      data =  deparse(substitute(data)),
      addEnv = addEnv
    ),
    settings = list(
      files=list(),
      meta=list(),
      labelCol="File"
    )
  )
  
  # (2) Prep an array of objects for the user-specified files  (if length(dataArray)>0)
  data_list <- getDataList(data)
  data_list_formatted <- formatDataList(data_list)
  if(length(data_list_formatted)>0){
    rSettings[["settings"]][["files"]] = c(rSettings[["settings"]][["files"]], data_list_formatted)
  }
  
  # (3) Prep an array of objects for the environment files (if addEnv=T)
  if(addEnv){
    if(length(ls(pos=1))>0){
      env_list <- ls(pos=1)[sapply(ls(pos=1), function(x) inherits(get(x), "data.frame"))]
      env_data_list <- getDataList(env_list)
      env_list_formatted <- formatDataList(env_data_list)
      if(length(env_list_formatted)>0){
        rSettings[["settings"]][["files"]] = c(rSettings[["settings"]][["files"]], env_list_formatted)
      } else {
        warning("No datasets to add from working environment;continuing with other user specified data sets.")
      } 
    }
    else{
      warning("No datasets to add from working environment; continuing with other user specified data sets.")
    }
  }
  
  # (4) load 20 datasets and put them in the environment (if demo=T)
  if(demo){
    demo_list = ls("package:datasets")[sapply(ls("package:datasets"), function(x) inherits(get(x), "data.frame"))]
    demo_data_list <- getDataList(demo_list)
    demo_list_formatted <- formatDataList(demo_data_list)
    rSettings[["settings"]][["files"]] = demo_list_formatted #ignores other settings
  }
  
  # (4) create widget
  htmlwidgets::createWidget(
    name = 'explorer',
    rSettings,
    package = 'codebook', 
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.fill=FALSE
    )
  )
}

#' Shiny bindings for Explorer
#'
#' Output and render functions for using codebook within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a codebook
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name explorer-shiny
#'
#' @export
explorerOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'explorer', width, height, package = 'codebook')
}

#' Shiny bindings for codebook
#'
#' Output and render functions for using codebook within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a codebook
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name explorer-shiny
#'
#' @export
codebookOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'codebook', width, height, package = 'codebook')
}

#' @rdname codebook-shiny
#' @export

renderCodebook <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, codebookOutput, env, quoted = TRUE)
}
