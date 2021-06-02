# install.packages("pacman")
pacman::p_load(shiny, vroom, ggplot2, plotly)

ui <- fluidPage(
  fileInput(inputId = "upload", "Upload file"),
  radioButtons("header", "Header", choices = c("True", "False"), inline = TRUE),
  radioButtons("p_type", "Plot type", choices = c("Point", "Line"), inline = TRUE),
  uiOutput("x_var_sel"),
  uiOutput("y_var_sel"),
  uiOutput("color_sel"),
  h3("Plot"),
  plotlyOutput("p1"),
  verbatimTextOutput("sum_stats")
)

server <- function(input, output, session) {
  
  # Read in data
  df <- reactiveValues(df = NULL)
  observeEvent(input$upload, {
    header <- ifelse(input$header, TRUE, FALSE)
    df$df <- vroom::vroom(input$upload$datapath, col_names = header)
    updateSelectInput(session, "y_var", selected = names(df$df)[2])
  })
  
  output$y_var_sel <- renderUI({
    req(!is.null(df$df))
    selectInput("y_var", "Choose Y variable", choices = colnames(df$df), selected = colnames(df$df)[2])
  })
  output$x_var_sel <- renderUI({
    req(!is.null(df$df))
    selectInput("x_var", "Choose X variable", choices = colnames(df$df))
  })
  output$color_sel <- renderUI({
    req(!is.null(df$df))
    selectInput("color", "Choose variable to color by:", choices = c("None", colnames(df$df)))
  })
  
  output$p1 <- renderPlotly({
    validate(
      need(!is.null(df$df), "Please upload a file")
    )
    validate(
      need(input$y_var %in% names(df$df), "Please select a y-variable")
    )
    p <- ggplot(df$df)
    if(input$p_type == "Point") {
      p <- p +
        {if(input$color == "None") geom_point(aes_string(input$x_var, input$y_var))} +
        {if(input$color != "None") {geom_point(aes_string(input$x_var, input$y_var, color = input$color))}} +
        {if(input$color != "None") {scale_colour_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))}}
    } else if( input$p_type == "Line") {
      p <- p + 
        geom_line(aes_string(input$x_var, input$y_var))
    }
    p <- p +
      theme_classic()
    ggplotly(p, dynamicTicks = TRUE)
  })
  
  output$sum_stats <- renderPrint({
    req(!is.null(df$df))
    summary(df$df)
  })
  
}

shinyApp(ui, server)
