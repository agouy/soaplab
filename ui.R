## UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Soap Lab"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Smart Recipe Generator", tabName = "optimiser"),
      menuItem("Recipe Maker", tabName = "recipe")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "optimiser",
        fluidRow(
          box(width = 12,
              title = "Available oils",
              selectInput(
                "oil_input2",
                "Add some ingredients:", choices = master_df$name_fr,
                multiple = TRUE
              )
          ),
          box(width = 12,
              title = "Magically created recipe",
              fluidRow(tableOutput("optim_table"))
          ),
          box(width = 12,
              title = "Soap quality",
              plotlyOutput("barplot_optim")
          )
        )
      ),
      tabItem(
        tabName = "recipe",
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(width = 12,
            selectInput("oil_input", "Select oil / fat to add:", choices = master_df$name_fr),
            numericInput("weight_input", "Weight (g):", 100, 0, 100000),
            actionButton("add_oil", "Add to the mixture!")
          ),
          box(width = 12,
            title = "Current mix",
            fluidRow(DT::dataTableOutput("myTable"))
          ),
          box(width = 12,
            title = "Soap quality",
            plotlyOutput("barplot_1")
          )
        )
      )
    )
  )
)
