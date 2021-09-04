server <- function(input, output) {

  buttonCounter <- 0L
  
  values <- reactiveValues()
  values$tab <- tibble() %>% rowwise() 
  proxyTable <- DT::dataTableProxy("tab")
  
  output$myTable <- DT::renderDataTable({
    df_out <- values$tab
    if(nrow(df_out) > 0) df_out <- values$tab %>% select(-id)
    DT::datatable(
      df_out,
      options = list(pageLength = 25, dom = "rt"),
      rownames = FALSE,
      escape = FALSE,
      editable = FALSE
    )
  })
  
  observeEvent(input$remove_button_Tab1, {
    myTable <- values$tab
    s <- as.numeric(strsplit(input$remove_button_Tab1, "_")[[1]][2])
    myTable <- filter(myTable, id != s)
    replaceData(proxyTable, myTable, resetPaging = FALSE)
    values$tab <- myTable
  })
  observeEvent(input$add_oil, {
    buttonCounter <<- buttonCounter + 1L
    myTable <- isolate(values$tab)
    myTable <- bind_rows(
      myTable,
      master_df %>% 
        filter(name_fr == input$oil_input) %>%
        select(name_fr) %>%
        mutate(weight = input$weight_input,
               id = buttonCounter,
               Remove = getRemoveButton(buttonCounter, idS = "", lab = "Tab1"))
    )
    replaceData(proxyTable, myTable, resetPaging = FALSE)
    values$tab <- myTable
  })
  
  
  output$barplot_1 <- renderPlotly({
    oil_names <- values$tab$name_fr
    weights <- values$tab$weight
    data <- get_metrics(oil_names, weights)
    fig <- barplot_metrics(data)
    fig
  })
  
  output$barplot_optim <- renderPlotly({
    if(length(input$oil_input2) < 2) return(NULL)
    df_out <- get_mixture(input$oil_input2)
    df <- current_mix()
    data <- get_metrics(input$oil_input2, df[input$oil_input2])
    
    fig <- barplot_metrics(data)
    fig
  })
  
  current_mix <- reactive({
    if(length(input$oil_input2) < 2) return(NULL)
    df_out <- get_mixture(input$oil_input2)
    return(df_out)
  })
  
  output$optim_table <- renderTable({
      df_out <- current_mix() 
      req(df_out)
      t(df_out)
    }, rownames = TRUE, colnames = FALSE)
  
}

