library(shiny)
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  shinyjs::useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "app.js")
  ),

  navbarPage(
    "EKQDA - Qualitative Data Analysis",
    id = "main_nav",

    #Documents tab
    tabPanel(
      "Documents",
      icon = icon("file-text"),
      fluidRow(
        column(3,
               wellPanel(
                 h4("Document Management"),
                 fileInput("import_files", "Import Documents",
                           multiple = TRUE, accept = c(".txt", ".csv")),
                 actionButton("create_doc", "Create New Document", class = "btn-primary"),
                 hr(),
                 h5("Project Info"),
                 verbatimTextOutput("project_info")
               )
        ),
        column(9,
               tabsetPanel(
                 id = "documents_tabs",
                 tabPanel(
                   "Document List",
                   h4("Documents in Project"),
                   DT::DTOutput("documents_table"),
                   br()
                 ),
                 tabPanel(
                   "Document Viewer",
                   h4("Document Content"),
                   uiOutput("document_viewer")
                 )
               )
        )
      )
    ),

    # Coding Tab
    tabPanel(
      "Coding",
      icon = icon("code"),
      fluidRow(
        column(3,
               wellPanel(
                 h4("Code Management"),
                 textInput("new_code_name", "Code Name"),
                 textInput("new_code_desc", "Description"),
                 colourpicker::colourInput("new_code_color", "Color", value = "#3366FF"),
                 actionButton("add_code", "Add Code", class = "btn-success"),
                 hr(),
                 h5("Existing Codes"),
                 DT::DTOutput("codes_table"),
                 # Add this for code selection
                 uiOutput("selected_code_info")
               )
        ),
        column(9,
               h4("Text Coding Interface"),
               # Document selector and coding controls
               fluidRow(
                 column(6,
                        selectInput("coding_doc", "Select Document", choices = NULL)
                 ),
                 column(6,
                        uiOutput("coding_controls")
                 )
               ),
               hr(),
               # Text display with coding capability
               uiOutput("text_coding_interface"),
               # Current codings for this document
               h4("Current Codings in this Document"),
               DT::DTOutput("document_codings_table")
        )
      )
    ),

    # Analysis Tab
    tabPanel(
      "Analysis",
      icon = icon("chart-bar"),
      tabsetPanel(
        id = "analysis_tabs",

        # Code Frequency Tab
        tabPanel(
          "Code Frequency",
          icon = icon("chart-column"),
          fluidRow(
            column(8,
                   h4("Code Frequency Distribution"),
                   plotOutput("code_frequency_plot", height = "400px")
            ),
            column(4,
                   wellPanel(
                     h4("Frequency Options"),
                     checkboxInput("show_empty_codes", "Show codes with zero uses", FALSE),
                     downloadButton("download_freq_csv", "Download as CSV", class = "btn-sm"),
                     downloadButton("download_freq_docx", "Download as Word", class = "btn-sm")
                   )
            )
          ),
          hr(),
          h4("Code Frequency Table"),
          DT::DTOutput("code_frequency_table")
        ),

        # Code Retrieval Tab - MOST IMPORTANT
        tabPanel(
          "Code Retrieval",
          icon = icon("search"),
          fluidRow(
            column(3,
                   wellPanel(
                     h4("Retrieval Options"),
                     selectInput("retrieval_code", "Select Code", choices = NULL),
                     checkboxGroupInput("retrieval_columns", "Include in export:",
                                        choices = c("Document" = "document",
                                                    "Code" = "code",
                                                    "Position" = "position",
                                                    "Memo" = "memo"),
                                        selected = c("document", "code", "memo")),
                     numericInput("max_retrieval", "Max results", value = 100, min = 1, max = 1000),
                     actionButton("run_retrieval", "Retrieve Coded Text", class = "btn-primary"),
                     hr(),
                     h5("Export Results"),
                     downloadButton("download_retrieval_csv", "CSV", class = "btn-sm"),
                     downloadButton("download_retrieval_txt", "Text File", class = "btn-sm"),
                     downloadButton("download_retrieval_docx", "Word Document", class = "btn-sm")
                   )
            ),
            column(9,
                   h4("Coded Text Segments"),
                   uiOutput("retrieval_summary"),
                   DT::DTOutput("retrieved_codings_table"),
                   uiOutput("retrieved_codings_detail")
            )
          )
        ),

        # Visualizations Tab
        tabPanel(
          "Visualizations",
          icon = icon("chart-pie"),
          fluidRow(
            column(12,
                   h4("Code Distribution"),
                   plotOutput("code_pie_chart", height = "350px")
            )
          ),
          fluidRow(
            column(12,
                   h4("Code Co-occurrence"),
                   p("Number of documents in which both codes have been applied."),
                   plotOutput("cooccurrence_heatmap", height = "400px")
            )
          )
        )
      )
    ),

    # Memos Tab
    tabPanel(
      "Memos",
      icon = icon("sticky-note"),
      fluidRow(
        column(4,
               wellPanel(
                 h4("Create New Memo"),
                 selectInput("memo_type", "Memo Type",
                             choices = c("Code Memo" = "code",
                                         "Document Memo" = "document",
                                         "General Memo" = "general")),
                 # Dynamic selection based on memo type
                 uiOutput("memo_target_selector"),
                 textInput("memo_title", "Memo Title", placeholder = "Enter memo title"),
                 textAreaInput("memo_content", "Memo Content", rows = 8,
                               placeholder = "Write your memo here...\n\nExamples:\n- Theoretical insights\n- Analytical decisions\n- Methodological notes\n- Connections between codes\n- Emerging patterns"),
                 actionButton("save_memo", "Save Memo", class = "btn-success", icon = icon("save")),
                 actionButton("clear_memo", "Clear", class = "btn-default")
               )
        ),
        column(8,
               h4("Your Memos"),
               # Memo filtering and search
               fluidRow(
                 column(6,
                        selectInput("filter_memo_type", "Filter by Type",
                                    choices = c("All Types" = "all", "Code Memos" = "code",
                                                "Document Memos" = "document", "General Memos" = "general"))
                 ),
                 column(6,
                        textInput("search_memos", "Search Memos", placeholder = "Search in titles and content...")
                 )
               ),
               # Memos list
               DT::DTOutput("memos_table"),
               # Selected memo detail view
               uiOutput("selected_memo_detail")
        )
      )
    )
  )
)

