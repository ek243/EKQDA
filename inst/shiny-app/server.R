server <- function(input, output, session) {

  `%>%` <- dplyr::`%>%`

  # The project is handed over by EKQDA::qual_launch_app()
  if (is.null(getOption("ekqda.project"))) {
    stop("No project found. Start the app with EKQDA::qual_launch_app().",
         call. = FALSE)
  }

  # Reactive values for app state
  app_state <- reactiveValues(
    project = getOption("ekqda.project"),
    documents = data.frame(),
    codes = data.frame(),
    codings = data.frame(),
    current_doc = NULL,
    selected_text = NULL
  )

  # Load initial data
  observe({
    app_state$documents <- load_documents()
    app_state$codes <- load_codes()
    app_state$codings <- load_codings()
  })

  # Load data functions
  load_documents <- function() {
    DBI::dbGetQuery(app_state$project$connection, "SELECT * FROM documents")
  }

  load_codes <- function() {
    DBI::dbGetQuery(app_state$project$connection, "SELECT * FROM codes")
  }

  load_codings <- function() {
    DBI::dbGetQuery(app_state$project$connection,
                    "SELECT c.*, cd.name as code_name, d.name as doc_name
                    FROM codings c
                    JOIN codes cd ON c.code_id = cd.id
                    JOIN documents d ON c.doc_id = d.id")
  }

  # Project info
  output$project_info <- renderText({
    docs_count <- nrow(app_state$documents)
    codes_count <- nrow(app_state$codes)
    codings_count <- nrow(app_state$codings)

    paste("Documents:", docs_count, "\n",
          "Codes:", codes_count, "\n",
          "Codings:", codings_count)
  })

  # Documents table
  output$documents_table <- DT::renderDT({
    req(app_state$documents)
    DT::datatable(
#      app_state$documents %>% select(id, name, created),
      dplyr::select(app_state$documents, id, name, created),
      selection = 'single',
      options = list(pageLength = 10)
    )
  })

  # Document viewer
  output$document_viewer <- renderUI({
    selected <- input$documents_table_rows_selected
    if (length(selected) == 0) {
      return(
        div(class = "well",
            h5("No document selected"),
            p("Select a document from the table to view its content")
        )
      )
    }

    doc <- app_state$documents[selected, ]
    app_state$current_doc <- doc

    tagList(
      h5("Document:", doc$name),
      div(
        style = "border: 1px solid #ddd; padding: 15px; max-height: 400px; overflow-y: auto;
               background-color: #f9f9f9; font-family: 'Courier New', monospace;",
        # Use preformatted text but with word wrap
        tags$div(style = "white-space: pre-wrap;", doc$content)
      ),
      p(style = "font-size: 12px; color: #666;",
        paste("Created:", doc$created))
    )
  })

  # Import documents
  observeEvent(input$import_files, {
    req(input$import_files)

    for (i in seq_len(nrow(input$import_files))) {
      EKQDA::qual_import_text(
        app_state$project,
        input$import_files$datapath[i],
        input$import_files$name[i]
      )
    }

    # Refresh data
    app_state$documents <- load_documents()
    showNotification("Documents imported successfully!", type = "message")
  })

  # Create New Document button handler
  observeEvent(input$create_doc, {
    showModal(
      modalDialog(
        title = "Create New Document",
        textInput("new_doc_name", "Document Name", placeholder = "Enter document name"),
        textAreaInput("new_doc_content", "Content", rows = 10,
                      placeholder = "Paste or type your document content here..."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_create_doc", "Create", class = "btn-primary")
        )
      )
    )
  })

  # Confirm document creation
  observeEvent(input$confirm_create_doc, {
    req(input$new_doc_name, input$new_doc_content)

    # Basic validation
    if (input$new_doc_name == "") {
      showNotification("Please enter a document name", type = "warning")
      return()
    }

    if (input$new_doc_content == "") {
      showNotification("Please enter some content", type = "warning")
      return()
    }

    tryCatch({
      # Insert directly into database
      DBI::dbExecute(
        app_state$project$connection,
        "INSERT INTO documents (name, content) VALUES (?, ?)",
        params = list(input$new_doc_name, input$new_doc_content)
      )

      # Refresh documents
      app_state$documents <- load_documents()

      # Close modal and show success message
      removeModal()
      showNotification("Document created successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error creating document:", e$message), type = "error")
    })
  })

  # Code management
  output$codes_table <- DT::renderDT({
    req(app_state$codes)
    DT::datatable(
#      app_state$codes %>% select(id, name, description, color),
      dplyr::select(app_state$codes, id, name, description, color),
      selection = 'single',
      options = list(pageLength = 10)
    )
  })

  # Add new code
  observeEvent(input$add_code, {
    req(input$new_code_name)

    EKQDA::qual_add_code(
      app_state$project,
      name = input$new_code_name,
      description = input$new_code_desc,
      color = input$new_code_color
    )

    # Clear inputs and refresh
    updateTextInput(session, "new_code_name", value = "")
    updateTextInput(session, "new_code_desc", value = "")
    app_state$codes <- load_codes()

    showNotification("Code added successfully!", type = "message")
  })

  # Text coding interface
  output$document_selector <- renderUI({
    docs <- app_state$documents
    if (nrow(docs) == 0) return(p("No documents available"))

    selectInput("coding_doc", "Select Document",
                choices = setNames(docs$id, docs$name))
  })

  output$text_coding_interface <- renderUI({
    req(input$coding_doc)

    doc_id <- as.numeric(input$coding_doc)
#    doc <- app_state$documents %>% filter(id == doc_id)
    doc <- dplyr::filter(app_state$documents, id == doc_id)

    if (nrow(doc) == 0) return(NULL)

    # Get existing codings for this document
#    doc_codings <- app_state$codings %>% filter(doc_id == !!doc_id)
    doc_codings <- dplyr::filter(app_state$codings, doc_id == !!doc_id)

    tagList(
      h5("Document Text - Select text to code it"),
      div(
        id = "codable-text",
        style = "border: 1px solid #ddd; padding: 15px; max-height: 500px; overflow-y: auto;
                 background-color: #f9f9f9;",
        # We'll use JavaScript for text selection - this is a simplified version
        pre(doc$content)
      ),

      # Current codings for this document
      h5("Existing Codings"),
      if (nrow(doc_codings) > 0) {
        DT::renderDT({
#          DT::datatable(doc_codings %>% select(code_name, text, start, end))
          DT::datatable(dplyr::select(doc_codings, code_name, text, start, end))
        })
      } else {
        p("No codings for this document yet.")
      }
    )
  })

  # Update document choices for coding
  observe({
    docs <- app_state$documents
    if (nrow(docs) > 0) {
      updateSelectInput(session, "coding_doc",
                        choices = setNames(docs$id, docs$name))
    } else {
      updateSelectInput(session, "coding_doc", choices = "No documents available")
    }
  })

  # Show selected code info
  output$selected_code_info <- renderUI({
    selected <- input$codes_table_rows_selected
    if (length(selected) == 0) return(NULL)

    code <- app_state$codes[selected, ]
    tagList(
      hr(),
      h5("Selected Code:"),
      div(style = paste0("background-color:", code$color, "; color: white; padding: 5px; border-radius: 3px;"),
          strong(code$name)
      ),
      p(code$description)
    )
  })

  # Coding controls
  output$coding_controls <- renderUI({
    selected_code <- input$codes_table_rows_selected
    selected_doc <- input$coding_doc

    if (length(selected_code) == 0 || selected_doc == "" || selected_doc == "No documents available") {
      return(p("Select a document and a code to start coding"))
    }

    tagList(
      h5("Coding Controls"),
      textInput("coding_memo", "Memo (optional)", placeholder = "Add a memo for this coding"),
      actionButton("apply_code_btn", "Apply Selected Code to Highlighted Text",
                   class = "btn-primary", icon = icon("tag")),
      actionButton("remove_coding_btn", "Remove Selected Coding",
                   class = "btn-warning", icon = icon("trash"))
    )
  })

  # Text coding interface
  output$text_coding_interface <- renderUI({
    req(input$coding_doc)
    if (input$coding_doc == "No documents available") return(p("No documents available for coding"))

    doc_id <- as.numeric(input$coding_doc)
    doc <- app_state$documents[app_state$documents$id == doc_id, ]

    if (nrow(doc) == 0) return(NULL)

    # Get existing codings for highlighting
    doc_codings <- app_state$codings[app_state$codings$doc_id == doc_id, ]

    tagList(
      h5("Document Text - Highlight text and click 'Apply Selected Code'"),
      div(
        id = "codable-text",
        style = "border: 1px solid #ddd; padding: 15px; max-height: 500px; overflow-y: auto;
               background-color: #f9f9f9; font-family: 'Courier New', monospace;",
        # We'll create a text area that allows selection
        uiOutput("highlighted_text_display")
      )
    )
  })

  # Display text with existing coding highlights
  output$highlighted_text_display <- renderUI({
    req(input$coding_doc)
    doc_id <- as.numeric(input$coding_doc)
    doc <- app_state$documents[app_state$documents$id == doc_id, ]
    if (nrow(doc) == 0) return(NULL)

    doc_codings <- app_state$codings[app_state$codings$doc_id == doc_id, ]
    text <- doc$content

    # If no codings, just return plain text
    if (nrow(doc_codings) == 0) {
      return(tags$div(style = "white-space: pre-wrap;", text))
    }

    # Create highlighted text (simplified version)
    # In a real implementation, you'd need more sophisticated text highlighting
    highlighted_text <- text
    for (i in 1:nrow(doc_codings)) {
      coding <- doc_codings[i, ]
      code_info <- app_state$codes[app_state$codes$id == coding$code_id, ]
      if (nrow(code_info) > 0) {
        # Simple highlighting - this is a basic implementation
        span_style <- paste0(
          "background-color:", code_info$color, ";",
          "color: white;",
          "padding: 1px 3px;",
          "margin: 0 1px;",
          "border-radius: 2px;",
          "cursor: pointer;",
          "title='", code_info$name, "'"
        )
        highlighted_text <- gsub(
          coding$text,
          paste0("<span style='", span_style, "' data-coding-id='", coding$id, "'>", coding$text, "</span>"),
          highlighted_text,
          fixed = TRUE
        )
      }
    }

    HTML(highlighted_text)
  })

  # Text selection handling using JavaScript and Shiny
  observeEvent(input$apply_code_btn, {
    req(input$coding_doc, input$codes_table_rows_selected)

    # For now, we'll use a modal to get the text selection
    # In a more advanced version, you'd use JavaScript to capture the exact selection
    showModal(
      modalDialog(
        title = "Apply Code to Text",
        textAreaInput("selected_text_input", "Selected Text to Code",
                      placeholder = "Paste the text you want to code here...",
                      rows = 3),
        numericInput("text_start", "Start Position", value = 1, min = 1),
        numericInput("text_end", "End Position", value = 100, min = 1),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_apply_code", "Apply Code", class = "btn-primary")
        )
      )
    )
  })

  # Apply the code to the selected text
  observeEvent(input$confirm_apply_code, {
    req(input$coding_doc, input$codes_table_rows_selected, input$selected_text_input)

    doc_id <- as.numeric(input$coding_doc)
    code_id <- app_state$codes[input$codes_table_rows_selected, "id"]
    selected_text <- input$selected_text_input
    start_pos <- input$text_start
    end_pos <- input$text_end

    tryCatch({
      # Apply the coding
      DBI::dbExecute(
        app_state$project$connection,
        "INSERT INTO codings (doc_id, code_id, start, end, text, memo) VALUES (?, ?, ?, ?, ?, ?)",
        params = list(doc_id, code_id, start_pos, end_pos, selected_text, input$coding_memo)
      )

      # Refresh codings
      app_state$codings <- load_codings()

      removeModal()
      showNotification("Code applied successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error applying code:", e$message), type = "error")
    })
  })

  # Display current codings for the document
  output$document_codings_table <- DT::renderDT({
    req(input$coding_doc)
    doc_id <- as.numeric(input$coding_doc)

    doc_codings <- app_state$codings[app_state$codings$doc_id == doc_id, ]

    if (nrow(doc_codings) == 0) {
      return(DT::datatable(data.frame(Message = "No codings for this document yet.")))
    }

    DT::datatable(
#      doc_codings %>% select(code_name, text, start, end, memo),
      dplyr::select(doc_codings, code_name, text, start, end, memo),
      selection = 'single',
      options = list(pageLength = 5)
    )
  })

  # Remove selected coding
  observeEvent(input$remove_coding_btn, {
    selected_coding <- input$document_codings_table_rows_selected
    if (length(selected_coding) == 0) {
      showNotification("Please select a coding to remove", type = "warning")
      return()
    }

    doc_id <- as.numeric(input$coding_doc)
    doc_codings <- app_state$codings[app_state$codings$doc_id == doc_id, ]
    coding_id <- doc_codings[selected_coding, "id"]

    tryCatch({
      DBI::dbExecute(
        app_state$project$connection,
        "DELETE FROM codings WHERE id = ?",
        params = list(coding_id)
      )

      app_state$codings <- load_codings()
      showNotification("Coding removed successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error removing coding:", e$message), type = "error")
    })
  })

  # Visualizations tab: code distribution
  output$code_pie_chart <- renderPlot({
    invisible(app_state$codings)  # re-draw whenever the codings change
    freq_data <- EKQDA::qual_code_frequency(app_state$project)
    freq_data <- freq_data[freq_data$frequency > 0, , drop = FALSE]

    if (nrow(freq_data) == 0) {
      return(plot_empty("No coding data available"))
    }

    ggplot2::ggplot(freq_data, ggplot2::aes(x = "", y = frequency, fill = name)) +
      ggplot2::geom_col(width = 1, colour = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(fill = "Code") +
      ggplot2::theme_void() +
      ggplot2::scale_fill_viridis_d()
  })

  # Visualizations tab: code co-occurrence (documents containing both codes)
  output$cooccurrence_heatmap <- renderPlot({
    invisible(app_state$codings)  # re-draw whenever the codings change
    cooc <- EKQDA::qual_code_cooccurrence(app_state$project, level = "document")

    if (nrow(cooc) < 2 || sum(cooc[upper.tri(cooc)]) == 0) {
      return(plot_empty("No co-occurring codes yet"))
    }

    cooc_long <- as.data.frame(as.table(cooc), stringsAsFactors = FALSE)
    names(cooc_long) <- c("code_x", "code_y", "n")

    ggplot2::ggplot(cooc_long, ggplot2::aes(x = code_x, y = code_y, fill = n)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = n)) +
      ggplot2::scale_fill_viridis_c(direction = -1) +
      ggplot2::labs(x = NULL, y = NULL, fill = "Documents") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })

  # Analysis reactive values
  analysis_state <- reactiveValues(
    code_frequency = data.frame(),
    retrieved_codings = data.frame(),
    export_data = data.frame()
  )

  # Update code choices for retrieval
  observe({
    codes <- app_state$codes
    if (nrow(codes) > 0) {
      updateSelectInput(session, "retrieval_code",
                        choices = setNames(codes$id, codes$name))
    }
  })

  # Code Frequency Analysis
  observe({
    # Calculate code frequency
    if (nrow(app_state$codings) > 0) {
      freq_data <- app_state$codings %>%
        dplyr::group_by(code_id, code_name) %>%
        dplyr::summarise(frequency = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(frequency))

      # Include codes with zero frequency if requested
      if (input$show_empty_codes) {
        all_codes <- app_state$codes %>%
          dplyr::select(code_id = id, code_name = name) %>%
          dplyr::left_join(freq_data, by = c("code_id", "code_name")) %>%
          dplyr::mutate(frequency = ifelse(is.na(frequency), 0, frequency)) %>%
          dplyr::arrange(dplyr::desc(frequency))

        analysis_state$code_frequency <- all_codes
      } else {
        analysis_state$code_frequency <- freq_data
      }
    }
  })

  # Code Frequency Plot
  output$code_frequency_plot <- renderPlot({
    req(analysis_state$code_frequency)

    if (nrow(analysis_state$code_frequency) == 0) {
      return(plot_empty("No coding data available"))
    }

    freq_data <- analysis_state$code_frequency %>%
      dplyr::slice_max(frequency, n = 15)

    ggplot2::ggplot(freq_data, ggplot2::aes(x = reorder(code_name, frequency), y = frequency, fill = code_name)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Code", y = "Frequency", title = "Most Frequent Codes") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_viridis_d()
  })

  # Code Frequency Table
  output$code_frequency_table <- DT::renderDT({
    req(analysis_state$code_frequency)

    display_data <- analysis_state$code_frequency %>%
      dplyr::select(Code = code_name, Frequency = frequency)

    DT::datatable(
      display_data,
      options = list(pageLength = 10, dom = 'tip')
    )
  })

  # CODE RETRIEVAL - CORE FUNCTIONALITY
  observeEvent(input$run_retrieval, {
    req(input$retrieval_code)

    code_id <- as.numeric(input$retrieval_code)
    max_results <- input$max_retrieval

    # Get coded segments for selected code
    retrieved <- app_state$codings %>%
      dplyr::filter(code_id == !!code_id) %>%
      dplyr::arrange(doc_name, start) %>%
      dplyr::slice_head(n = max_results) %>%
      dplyr::mutate(
        text_preview = ifelse(nchar(text) > 100,
                              paste0(substr(text, 1, 100), "..."),
                              text),
        position_info = paste0("Chars ", start, "-", end)
      )

    analysis_state$retrieved_codings <- retrieved
  })

  # Retrieval summary
  output$retrieval_summary <- renderUI({
    retrieved <- analysis_state$retrieved_codings
    code_name <- input$retrieval_code

    if (nrow(retrieved) == 0) {
      return(p("No coded segments found for selected code."))
    }

    tagList(
      p(strong("Found"), strong(nrow(retrieved)), strong("coded segments for:"),
        strong(app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"])),
      p("Documents:", length(unique(retrieved$doc_name)))
    )
  })

  # Retrieved codings table
  output$retrieved_codings_table <- DT::renderDT({
    retrieved <- analysis_state$retrieved_codings

    if (nrow(retrieved) == 0) {
      return(DT::datatable(data.frame(Message = "Run retrieval to see coded segments")))
    }

    display_data <- retrieved %>%
      dplyr::select(Document = doc_name, Text = text_preview, Position = position_info, Memo = memo)

    DT::datatable(
      display_data,
      selection = 'single',
      options = list(pageLength = 10, dom = 'tip'),
      rownames = FALSE
    )
  })

  # Retrieved coding detail
  output$retrieved_codings_detail <- renderUI({
    selected <- input$retrieved_codings_table_rows_selected
    if (length(selected) == 0) return(NULL)

    coding <- analysis_state$retrieved_codings[selected, ]

    tagList(
      hr(),
      h5("Full Text Segment"),
      div(
        style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; border-radius: 4px;",
        tags$div(style = "white-space: pre-wrap; font-family: 'Courier New', monospace;",
                 coding$text)
      ),
      p(style = "font-size: 12px; color: #666;",
        paste("From:", coding$doc_name, "| Position:", coding$start, "-", coding$end))
    )
  })

  # EXPORT FUNCTIONS

  # Download handlers for code frequency
  output$download_freq_csv <- downloadHandler(
    filename = function() {
      paste("code-frequency-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysis_state$code_frequency, file, row.names = FALSE)
    }
  )

  # Download handler for retrieved codings - CSV
  output$download_retrieval_csv <- downloadHandler(
    filename = function() {
      code_name <- app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"]
      paste("coded-text-", gsub(" ", "-", code_name), "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- analysis_state$retrieved_codings %>%
        dplyr::select(Code = code_name, Document = doc_name, Text = text,
                      Start = start, End = end, Memo = memo)
      write.csv(data, file, row.names = FALSE)
    }
  )

  # Download handler for retrieved codings - Text file
  output$download_retrieval_txt <- downloadHandler(
    filename = function() {
      code_name <- app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"]
      paste("coded-text-", gsub(" ", "-", code_name), "-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- analysis_state$retrieved_codings
      code_name <- app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"]

      # Create a nicely formatted text file
      content <- paste0(
        "CODE RETRIEVAL REPORT\n",
        "Code: ", code_name, "\n",
        "Generated: ", Sys.Date(), "\n",
        "Total segments: ", nrow(data), "\n",
        "Documents: ", paste(unique(data$doc_name), collapse = ", "), "\n",
        "\n", strrep("=", 50), "\n\n"
      )

      for (i in seq_len(nrow(data))) {
        segment <- data[i, ]
        content <- paste0(
          content,
          "SEGMENT ", i, " of ", nrow(data), "\n",
          "Document: ", segment$doc_name, "\n",
          "Position: ", segment$start, "-", segment$end, "\n",
          ifelse(!is.na(segment$memo) && segment$memo != "",
                 paste0("Memo: ", segment$memo, "\n"), ""),
          "Text: ", segment$text, "\n",
          "\n", strrep("-", 30), "\n\n"
        )
      }

      writeLines(content, file)
    }
  )

  # Download handler for retrieved codings - Word document
  output$download_retrieval_docx <- downloadHandler(
    filename = function() {
      code_name <- app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"]
      paste("coded-text-", gsub(" ", "-", code_name), "-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      data <- analysis_state$retrieved_codings
      code_name <- app_state$codes[app_state$codes$id == as.numeric(input$retrieval_code), "name"]

      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Qualitative analysis report", style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Code:", code_name))
      doc <- officer::body_add_par(doc, paste("Date:", Sys.Date()))
      doc <- officer::body_add_par(
        doc, paste("Total coded segments:", nrow(data)))

      for (i in seq_len(nrow(data))) {
        segment <- data[i, ]
        doc <- officer::body_add_par(
          doc, paste0("Segment ", i, " (", segment$doc_name, ", characters ",
                      segment$start, "-", segment$end, ")"),
          style = "heading 2")
        doc <- officer::body_add_par(doc, segment$text)
        if (!is.na(segment$memo) && segment$memo != "") {
          doc <- officer::body_add_par(doc, paste("Analytical note:", segment$memo))
        }
      }

      print(doc, target = file)
    }
  )

  # Download handler for code frequency - Word document
  output$download_freq_docx <- downloadHandler(
    filename = function() {
      paste("code-frequency-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      freq <- analysis_state$code_frequency
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Code frequency", style = "heading 1")
      if (nrow(freq) > 0) {
        table_data <- data.frame(
          Code = freq$code_name,
          Frequency = freq$frequency
        )
        doc <- officer::body_add_table(doc, table_data, style = "table_template")
      } else {
        doc <- officer::body_add_par(doc, "No coding data available.")
      }
      print(doc, target = file)
    }
  )

  # Helper function for empty plots
  plot_empty <- function(message) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = message, size = 6) +
      ggplot2::theme_void()
  }

  # Memos UI
  output$code_memos_ui <- renderUI({
    tagList(
      selectInput("memo_code", "Select Code",
                  choices = setNames(app_state$codes$id, app_state$codes$name)),
      textAreaInput("code_memo", "Memo Text", rows = 6),
      actionButton("save_code_memo", "Save Memo", class = "btn-primary")
    )
  })

  # Reactive values for memos
  memo_state <- reactiveValues(
    memos = data.frame(),
    editing_memo = NULL
  )

  # Load memos from database
  load_memos <- function() {
    DBI::dbGetQuery(app_state$project$connection, "
    SELECT m.*,
           CASE
             WHEN m.type = 'code' THEN c.name
             WHEN m.type = 'document' THEN d.name
             ELSE 'General'
           END as target_name
    FROM memos m
    LEFT JOIN codes c ON m.type = 'code' AND m.target_id = c.id
    LEFT JOIN documents d ON m.type = 'document' AND m.target_id = d.id
    ORDER BY m.updated DESC
  ")
  }

  # Update memos when needed
  observe({
    memo_state$memos <- load_memos()
  })

  # Dynamic target selector based on memo type
  output$memo_target_selector <- renderUI({
    memo_type <- input$memo_type

    if (memo_type == "code") {
      choices <- setNames(app_state$codes$id, app_state$codes$name)
      if (length(choices) == 0) {
        return(p("No codes available. Create codes first.", style = "color: red;"))
      }
      selectInput("memo_target_id", "Select Code", choices = choices)

    } else if (memo_type == "document") {
      choices <- setNames(app_state$documents$id, app_state$documents$name)
      if (length(choices) == 0) {
        return(p("No documents available. Import documents first.", style = "color: red;"))
      }
      selectInput("memo_target_id", "Select Document", choices = choices)

    } else {
      # General memo - no target needed
      return(NULL)
    }
  })

  # Save memo
  observeEvent(input$save_memo, {
    req(input$memo_title, input$memo_content)

    if (trimws(input$memo_title) == "") {
      showNotification("Please enter a memo title", type = "warning")
      return()
    }

    if (trimws(input$memo_content) == "") {
      showNotification("Please enter memo content", type = "warning")
      return()
    }

    # For code and document memos, require a target
    target_id <- NULL
    if (input$memo_type %in% c("code", "document")) {
      if (is.null(input$memo_target_id)) {
        showNotification("Please select a target for this memo type", type = "warning")
        return()
      }
      target_id <- as.numeric(input$memo_target_id)
    }

    tryCatch({
      if (is.null(memo_state$editing_memo)) {
        # Create new memo - handle NULL target_id properly
        if (is.null(target_id)) {
          # General memo - use NULL in SQL
          DBI::dbExecute(
            app_state$project$connection,
            "INSERT INTO memos (type, target_id, title, content) VALUES (?, NULL, ?, ?)",
            params = list(input$memo_type, trimws(input$memo_title), trimws(input$memo_content))
          )
        } else {
          # Code or document memo - include target_id
          DBI::dbExecute(
            app_state$project$connection,
            "INSERT INTO memos (type, target_id, title, content) VALUES (?, ?, ?, ?)",
            params = list(input$memo_type, target_id, trimws(input$memo_title), trimws(input$memo_content))
          )
        }
        showNotification("Memo created successfully!", type = "message")
      } else {
        # Update existing memo - handle NULL target_id properly
        if (is.null(target_id)) {
          DBI::dbExecute(
            app_state$project$connection,
            "UPDATE memos SET title = ?, content = ?, target_id = NULL, updated = CURRENT_TIMESTAMP WHERE id = ?",
            params = list(trimws(input$memo_title), trimws(input$memo_content), memo_state$editing_memo)
          )
        } else {
          DBI::dbExecute(
            app_state$project$connection,
            "UPDATE memos SET title = ?, content = ?, target_id = ?, updated = CURRENT_TIMESTAMP WHERE id = ?",
            params = list(trimws(input$memo_title), trimws(input$memo_content), target_id, memo_state$editing_memo)
          )
        }
        showNotification("Memo updated successfully!", type = "message")
        memo_state$editing_memo <- NULL
      }

      # Refresh memos and clear form
      memo_state$memos <- load_memos()
      updateTextInput(session, "memo_title", value = "")
      updateTextAreaInput(session, "memo_content", value = "")

    }, error = function(e) {
      showNotification(paste("Error saving memo:", e$message), type = "error")
    })
  })

  # Clear memo form
  observeEvent(input$clear_memo, {
    updateTextInput(session, "memo_title", value = "")
    updateTextAreaInput(session, "memo_content", value = "")
    memo_state$editing_memo <- NULL
  })

  # Filtered memos table
  output$memos_table <- DT::renderDT({
    memos <- memo_state$memos

    # Apply filters
    if (input$filter_memo_type != "all") {
      memos <- memos[memos$type == input$filter_memo_type, ]
    }

    # Apply search
    if (!is.null(input$search_memos) && input$search_memos != "") {
      search_term <- tolower(input$search_memos)
      memos <- memos[
        grepl(search_term, tolower(memos$title)) |
          grepl(search_term, tolower(memos$content)) |
          grepl(search_term, tolower(memos$target_name)),
      ]
    }

    # Prepare display data
    display_data <- memos %>%
      dplyr::select(id, type, title, target_name, created, updated) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          type == "code" ~ "Code Memo",
          type == "document" ~ "Document Memo",
          type == "general" ~ "General Memo"
        ),
        preview = ifelse(nchar(title) > 30, paste0(substr(title, 1, 30), "..."), title)
      ) %>%
      dplyr::select(id, Type = type, Title = preview, Target = target_name, Created = created, Updated = updated)

    DT::datatable(
      display_data,
      selection = 'single',
      options = list(
        pageLength = 10,
        dom = 'tip',
        columnDefs = list(
          list(targets = 0, visible = FALSE)  # Hide ID column
        )
      )
    )
  })

  # Selected memo detail view
  output$selected_memo_detail <- renderUI({
    selected <- input$memos_table_rows_selected
    if (length(selected) == 0) {
      return(
        div(class = "well",
            h5("No memo selected"),
            p("Select a memo from the table to view its content")
        )
      )
    }

    memo_id <- memo_state$memos[selected, "id"]
    memo <- memo_state$memos[memo_state$memos$id == memo_id, ]

    tagList(
      hr(),
      h4("Memo Details"),
      fluidRow(
        column(6,
               strong("Title: "), memo$title,
               br(),
               strong("Type: "),
               dplyr::case_when(
                 memo$type == "code" ~ "Code Memo",
                 memo$type == "document" ~ "Document Memo",
                 memo$type == "general" ~ "General Memo"
               ),
               br(),
               strong("Target: "), ifelse(is.na(memo$target_name) | memo$target_name == "General", "None", memo$target_name)
        ),
        column(6,
               strong("Created: "), format(as.POSIXct(memo$created), "%Y-%m-%d %H:%M"),
               br(),
               strong("Last Updated: "), format(as.POSIXct(memo$updated), "%Y-%m-%d %H:%M")
        )
      ),
      hr(),
      h5("Content"),
      div(
        style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; border-radius: 4px;",
        tags$div(style = "white-space: pre-wrap;", memo$content)
      ),
      br(),
      actionButton("edit_memo", "Edit Memo", class = "btn-primary", icon = icon("edit")),
      actionButton("delete_memo", "Delete Memo", class = "btn-danger", icon = icon("trash"))
    )
  })

  # Edit memo
  observeEvent(input$edit_memo, {
    selected <- input$memos_table_rows_selected
    if (length(selected) == 0) return()

    memo_id <- memo_state$memos[selected, "id"]
    memo <- memo_state$memos[memo_state$memos$id == memo_id, ]

    # Populate form with memo data
    updateSelectInput(session, "memo_type", selected = memo$type)
    updateTextInput(session, "memo_title", value = memo$title)
    updateTextAreaInput(session, "memo_content", value = memo$content)

    # Set editing state
    memo_state$editing_memo <- memo_id

    showNotification("Memo loaded for editing. Make changes and click 'Save Memo'.", type = "message")
  })

  # Delete memo
  observeEvent(input$delete_memo, {
    selected <- input$memos_table_rows_selected
    if (length(selected) == 0) return()

    memo_id <- memo_state$memos[selected, "id"]
    memo_title <- memo_state$memos[selected, "title"]

    showModal(
      modalDialog(
        title = "Confirm Delete",
        paste("Are you sure you want to delete the memo:", memo_title, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_memo", "Delete", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_delete_memo, {
    selected <- input$memos_table_rows_selected
    if (length(selected) == 0) return()

    memo_id <- memo_state$memos[selected, "id"]

    tryCatch({
      DBI::dbExecute(
        app_state$project$connection,
        "DELETE FROM memos WHERE id = ?",
        params = list(memo_id)
      )

      memo_state$memos <- load_memos()
      removeModal()
      showNotification("Memo deleted successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error deleting memo:", e$message), type = "error")
    })
  })
}

