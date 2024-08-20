###################################################################
# This shiny app allows the user to rapidly create comparative plots,
# display the data head, and generate a data frame summary.
#
# Supports all input formats that the R package "rio" can handle
# Maikol Diasparra
# August 13, 2024
##################################################################


# =======================================================
# Packages
# ======================================================+
library(shiny)
library(shinythemes)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(rio) # install_formats() #to install all format for rio
library(purrr)
library(summarytools)
library(shinyjs)

# =======================================================
# Functions & options
# ======================================================+

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

packageVersion("Shiny")

if (Sys.getenv("SHINY_PORT") == "") {
  options(shiny.maxRequestSize = 10000 * 1024^2)
}

# Creating a not in operator:
`%notin%` <- purrr::negate(`%in%`)


# Filtering the DB
filterDB <- function(db, XVars, CatVars) {
  filter_conditions <- lapply(seq_along(XVars), function(j, XVars, CatVars) {
    list(col = XVars[j], value = gsub('"', "'", eval(parse(text = paste0("c(", paste0("'", CatVars[[j]], collapse = "',"), "')", colapse = ""))), fixed = TRUE))
  }, XVars, CatVars)


  my_func <-
    compose(!!!map(
      filter_conditions,
      function(f) function(dat) filter(dat, !!sym(f$col) %in% f$value)
    ))

  db2 <- db %>%
    my_func()

  return(db2)
}

# ======================================================
# Shiny app (body)
# ======================================================


ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel("EasyPlot"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      p("Import most used file formats (e.g., .csv, .xlsx, .rds)"),
      fileInput("InFile", "Upload a single file", accept = c(".csv", ".xlsx", ".rds")),
      # actionButton("editCols", "Edit Column Names"), # work in progress!
      h5("Main variables selection:"),
      selectInput("x_var", "X-axis", choices = NULL, multiple = FALSE, selectize = FALSE),
      selectInput("y_var", "Y-axis", choices = NULL, multiple = FALSE, selectize = FALSE),
      selectInput("facet_x", "Panel(s) by column", choices = NULL, multiple = FALSE, selectize = FALSE),
      selectInput("facet_y", "Panel(s) by row", choices = NULL, multiple = FALSE, selectize = FALSE),
      selectInput("legend", "Legend", choices = NULL, multiple = FALSE, selectize = FALSE),
      h5("Data filtering:"),
      uiOutput("dim"),
      uiOutput("levels1"),
      actionButton("getImage", "Show the plot"),
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      actionButton("close", "Close window"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        tabsetPanel(
          tabPanel("Uploaded Data (the first 10 rows)", DTOutput("Data1")),
          tabPanel("Summary Stats (before applying filters)", htmlOutput("Stats")),
          tabPanel("Plot", plotOutput("plot", height="1000px")),
          tabPanel(
            "About",
            br(),
            h2("EasyPlot 1.0.1"),
            p("Created with R Shiny"),
            p("This shiny app allows the user to rapidly create comparative plots, display the data head, and generate a data frame summary."),
            p("2024 August")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  Data <- reactiveVal(data.frame())

  observeEvent(input$InFile, {
    file <- input$InFile
    InData <- rio:::import(file$datapath)
    Data(InData)
    updateSelectInput(session, "x_var", "X-axis",
      choices = c("", names(Data())), selected = ""
    )
    updateSelectInput(session, "y_var", "Y-axis",
      choices = c("", names(Data())), selected = ""
    )
  })

  observeEvent(input$editCols, {
    showModal(modalDialog(
      title = "Edit Column Names",
      textInput("newNames", "Enter new column names (comma separated)", value = paste(names(Data()), collapse = ", ")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveCols", "Save")
      )
    ))
  })

  observeEvent(input$saveCols, {
    new_names <- unlist(strsplit(input$newNames, ",\\s*"))
    if (length(new_names) == ncol(Data())) {
      colnames(Data()) <- new_names
      updateSelectInput(session, "x_var", "X-axis",
        choices = new_names
      )
      updateSelectInput(session, "y_var", "Y-axis",
        choices = new_names
      )
      removeModal()
    } else {
      showNotification("The number of new names must match the number of columns.", type = "error")
    }
  })

  output$Stats <- renderUI({
    inFile <- input$InFile
    if (is.null(inFile)) {
      return(NULL)
    } else {
      print(dfSummary(Data()),
        method = "render",
        Data.frame = inFile[[1]]
      )
    }
  })

  output$Data1 <- renderDT(
    {
      req(Data())
      datatable(head(Data(), n = 10), editable = TRUE)
    },
    server = FALSE
  )

  output$dim <- renderUI({
    b <- names(Data()) # [sapply(Data(), class) %notin% c("numeric")]
    selectInput("x", "Select the variable(s) to filter. Otherwise, leave it empty.", choices = c(NULL, b), selected = NULL, multiple = TRUE)
  })

  output$levels1 <- renderUI({
    if (is.null(input$x) || "NONE" %in% input$x) {
      return(NULL)
    } else {
      lapply(seq_along(input$x), function(j) {
        opts <- unique(Data()[[input$x[j]]])
        selectInput(
          inputId = paste0("range", j),
          label = paste0("Select level of ", input$x[j]),
          choices = opts,
          multiple = TRUE
        )
      })
    }
  })
  observeEvent(
    Data(),
    updateSelectInput(session,
      "facet_x", "Panel(s) by row",
      choices = c("", names(Data())), selected = ""
    )
  )

  observeEvent(
    Data(),
    updateSelectInput(session,
      "facet_y", "Panel(s) by column",
      choices = c("", names(Data())), selected = ""
    )
  )
  observeEvent(
    Data(),
    updateSelectInput(session,
      "legend", "Legend",
      choices = c("", names(Data())), selected = ""
    )
  )
  filteredFull <- reactive({
    if (is.null(input$x)) {
      return(Data())
    } else {
      ilevels <- lapply(seq_along(input$x), function(j) {
        eval(parse(text = (paste0("input$range", j))))
      })
      db <- filterDB(Data(), input$x, ilevels)
      return(db)
    }
  })

  observeEvent(
    input$getImage,
    {
      req(input$x_var, input$y_var)

      output$plot <- renderPlot({
        datos <- filteredFull()

   #     options(repr.plot.width = 15, repr.plot.height = 15)

        # Define the facetting
        facet_option <- NULL

        if (input$facet_x != "" & input$facet_y != "") {
          datos <- datos %>%
            mutate(
              facetx = get(input$facet_x),
              facety = get(input$facet_y)
            )
          facet_option <- facet_grid(facety ~ facetx)
        } else if (input$facet_x != "" & input$facet_y == "") {
          datos <- datos %>%
            mutate(facetx = get(input$facet_x))
          facet_option <- facet_grid(~facetx)
        } else if (input$facet_x == "" & input$facet_y != "") {
          datos <- datos %>%
            mutate(facety = get(input$facet_y))
          facet_option <- facet_wrap(vars(facety))
        }

        # Plot construction
        if (input$x_var != "" & input$y_var != "") {
          if (input$legend == "") {
            graph <- ggplot(datos, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
              geom_line(linewidth = 1.2) +
              geom_point() +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
          } else {
            datos <- datos %>%
              mutate(Scenarios = get(input$legend))
            graph <- ggplot(datos, aes(x = !!sym(input$x_var), y = !!sym(input$y_var), color = Scenarios, group = Scenarios)) +
              geom_line(linewidth = 1.2) +
              geom_point() +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)) +
              theme(legend.position = "bottom",legend.text=element_text(size=15))
          }
          # Apply facet options if present
          if (!is.null(facet_option)) {
            graph <- graph + facet_option
          }
          graph
        } else {
          showNotification("Please select X and Y.", type = "message")
        }
      })
    }
  )

  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
}

# Run the application

shinyApp(ui = ui, server = server)
