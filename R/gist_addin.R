#' @import shiny miniUI
gist_addin <- function() {
  context <- rstudioapi::getSourceEditorContext()
  
  if (context$path != "") {
    initial_filename <- basename(context$path)
  } else {
    initial_filename <- ""
  }
  
  ui <- miniPage(
    miniContentPanel(
      textInput("filename", 
                "Filename:", value = initial_filename),
      textInput("description", 
                "Description:"),
      radioButtons("type", "Type:",
                   choices = c("Secret", "Public"),
                   selected = "Secret"),
      actionButton("go", "Create")
  ))
  
  server <- function(input, output, session) {
    observeEvent(input$go,{
      
      public <- ifelse(input$type == "public", TRUE, FALSE)
      
      filename <- input$filename
      filename <- ifelse(grepl("\\.R|\\.r$", filename),
                         filename,
                         paste0(filename, ".R")
                         )
  
      gist<- gistr::gist_create(filename = filename,
                         code = paste0(context$contents, collapse = "\n"),
                         description = input$description,
                         public = public)
      stopApp(invisible(NULL))
    })
  }
  
   runGadget(ui, server, viewer = dialogViewer("Create gist", 
                                               width = 400,
                                               height = 300))
}