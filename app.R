library(bs4Dash)
library(readxl)
library(DT)
library(dplyr)
library(plotly)

getRemoveButton <- function(n, idS = "", lab = "Pit") {
  if (stringr::str_length(idS) > 0) idS <- paste0(idS, "-")
  ret <- shinyInput(actionButton, n,
                    'button_', label = "Remove",
                    onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab))
  return (ret)
}

shinyInput <- function(FUN, n, id, ses, ...) {
  as.character(FUN(paste0(id, n), ...))
}

master_df <- readxl::read_xlsx("./www/oils.xlsx")
get_hardness <- function(df) df$lauric + df$myristic + df$palmitic + df$stearic
get_cleansing <- function(df) df$lauric + df$myristic
get_longevity <- function(df) df$palmitic + df$stearic
get_conditioning <- function(df) df$oleic + df$ricinoleic + df$linoleic + df$linolenic
get_bubbliness <- function(df) df$lauric + df$myristic + df$ricinoleic
get_creaminess <- function(df) df$palmitic + df$stearic + df$ricinoleic

get_c <- function(thetas) return(1 / (sum(sapply(thetas, exp)) + 1))

get_x <- function(thetas) {
  cst <- get_c(thetas)
  xs <- cst * exp(thetas)
  return(c(xs, cst))
}

optim_fn <- function(par, df_tmp, ideal) {
  props <- get_x(par)
  res <- c(
    sum(get_hardness(df_tmp) * props),
    sum(get_cleansing(df_tmp) * props),
    sum(get_longevity(df_tmp) * props),
    sum(get_conditioning(df_tmp) * props),
    sum(get_bubbliness(df_tmp) * props),
    sum(get_creaminess(df_tmp) * props)
  )
  SS <- sum((res - ideal)^2)
  return(SS)
}

get_mixture <- function(oil_names) {
  
  oil <- sort(oil_names)
  df_tmp <-   master_df %>% filter(name_fr %in% oil)
  
  low <- c(29, 12, 25, 44, 14, 16)
  med <- c(54, 22, 50, 69, 46, 48)
  ideal <- (low + med) / 2
  
  n_props <- length(oil)
  par <- rep(1 / n_props, n_props)
  
  thetas <- log(par[- n_props] / par[n_props])

  optim_par <- optim(thetas, fn=optim_fn, gr=NULL, df_tmp, ideal, method=c("SANN"))
  
  best_val <- get_x(optim_par$par)
  names(best_val) <- paste0("Percentage_", oil)
  
  res <- c(
    hardness = sum(get_hardness(df_tmp) * best_val),
    cleansing = sum(get_cleansing(df_tmp) * best_val),
    longevity = sum(get_longevity(df_tmp) * best_val),
    conditioning = sum(get_conditioning(df_tmp) * best_val),
    bubbliness = sum(get_bubbliness(df_tmp) * best_val),
    creaminess = sum(get_creaminess(df_tmp) * best_val)
  )
  
  best_val <- round(100 * best_val)
  
  df_out <- data.frame(t(c(best_val, res)))
  return(df_out)
}


ui <- dashboardPage(
  dashboardHeader(title = "Mon savon maker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recipe", tabName = "recipe"),
      menuItem("Optimiser", tabName = "optimiser")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "recipe",
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(
            selectInput("oil_input", "Select oil / fat to add:", choices = master_df$name_fr),
            numericInput("weight_input", "Weight (g):", 100, 0, 100000),
            actionButton("add_oil", "Add to the mixture!")
          ),
          box(
            title = "Current mix",
            fluidRow(DT::dataTableOutput("myTable"))
          ),
          box(
            title = "Soap quality",
            plotlyOutput("barplot_1")
          )
        )
      ),
      
      tabItem(
        tabName = "optimiser",
        fluidRow(
          box(
            selectInput(
              "oil_input2",
              "Select oil / fat to add:", choices = master_df$name_fr,
              multiple = TRUE
            ),
            actionButton("run_optim", "Do the magic!")
          ),
          box(
            title = "Magically created recipe",
            fluidRow(DT::dataTableOutput("optim_table"))
          )
      )
      )
    )
  )
)

server <- function(input, output) {
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  buttonCounter <- 0L
  
  values <- reactiveValues()
  values$tab <- tibble() %>%
    rowwise() 
  proxyTable <- DT::dataTableProxy("tab")
  
  output$myTable <- DT::renderDataTable({
    df_out <- values$tab
    if(nrow(df_out) > 0) df_out <- values$tab %>% select(-id)
    DT::datatable(df_out,
                  options = list(pageLength = 25,
                                 dom        = "rt"),
                  rownames = FALSE,
                  escape   = FALSE,
                  editable = FALSE)
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

    
    
    filtered <- master_df %>% filter(name_fr %in% values$tab$name_fr)
    weights <- values$tab$weight
    perc <- weights/sum(weights)
    
    res <- data.frame(
      hardness = get_hardness(filtered),
      cleansing = get_cleansing(filtered),
      longevity = get_longevity(filtered),
      conditioning = get_conditioning(filtered),
      bubbliness = get_bubbliness(filtered),
      creaminess = get_creaminess(filtered)
    )
    
    
    
    param <- c("hardness","cleansing", "longevity", "conditioning","bubbliness","creaminess")
    low <- c(29, 12, 25, 44, 14, 16)
    med <- c(54, 22, 50, 69, 46, 48) - low
    high <- 100 - (med + low)
    
    data <- data.frame(param, low, med, high)
    
    data$obs <- apply(res[, data$param], 2, function(x) sum(perc * x))
    
    data <- data %>% arrange(param)
    
    fig <- plot_ly(data, x = ~param)
    fig <- fig %>% add_trace(
      y = ~low,
      marker = list(color = "lightgrey"),
      type = 'bar',
      name = 'boo'
    )
    fig <- fig %>% add_trace(
      y = ~med,
      marker = list(color = "purple"),
      type = 'bar',
      name = 'nice'
    )
    fig <- fig %>% add_trace(
      y = ~high,
      marker = list(color = "lightgrey"),
      type = 'bar',
      name = 'boo'
    )
    
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
    fig <- fig %>% add_trace(
      y = ~obs, name = 'mon savon',
      marker=list(color = "firebrick"), type = 'scatter', mode = "lines+markers"
    )
    
    
    fig
    
  })
  
  
  output$optim_table <- DT::renderDataTable({
    if(length(input$oil_input2) < 2) return(NULL)
    df_out <- get_mixture(input$oil_input2) 

    # if(nrow(df_out) > 0) df_out <- values$tab %>% select(-id)
    DT::datatable(t(df_out),
                  options = list(pageLength = 25,
                                 dom        = "rt"),
                  rownames = TRUE,
                  escape   = FALSE,
                  editable = FALSE)
  })
  
}

shinyApp(ui, server)