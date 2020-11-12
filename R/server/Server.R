sapply(list.files(pattern="[.]R$", path="R/server/function", full.names=TRUE), source);
#Ref :https://github.com/benmarwick/Interactive_PCA_Explorer
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
Server <- function(input, output, session) {
  # ======================== Variables ============================================
  # ------------------------- PCA --------------------------------
  values <- reactiveValues(check_file = NULL, check_img = NULL)
  # ----------------------- EigenFace --------------------------
  index1 <- reactiveVal(1)
  index2 <- reactiveVal(1)
  index3 <- reactiveVal(1)
  # ====================================================================================

  # ======================== Event ============================================
  # ------------------------- PCA --------------------------------
  observeEvent(input$dataset_btn_pca, {
    values$check_file = FALSE
    values$check_img = FALSE
  })
  observeEvent(input$file_btn_pca, {
    values$check_file = TRUE
    values$check_img = FALSE
  })
  # ----------------------- EigenFace --------------------------
  observeEvent(input$dataset_btn_eig, {
    values$check_img = TRUE
    values$check_file = FALSE
  })
  observeEvent(input$previous_1, {
    index1(max(index1() - 1, 1))
  })
  observeEvent(input$next_1, {
    if (is.null(imgInput()) == FALSE) {
      df <- imgInput()
      df <- df$df
      max_page <- as.numeric(ceiling(nrow(df) / 100))
      index1(min(index1() + 1, max_page))
    }
  })
  observeEvent(input$previous_2, {
    index2(max(index2() - 1, 1))
  })
  observeEvent(input$next_2, {
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        pca_img <- pcaObject$eigenfaces
        max_page <-
          as.numeric(ceiling(ncol(pcaObject$eigenfaces) / 100))
        index2(min(index2() + 1, max_page))
      }
      
      
    }
  })
  observeEvent(input$previous_3, {
    index3(max(index3() - 1, 1))
  })
  observeEvent(input$next_3, {
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        pca_img <- pcaObject$eigenfaces
        max_page <-
          as.numeric(ceiling(ncol(pcaObject$eigenfaces) / 100))
        index3(min(index3() + 1, max_page))
      }
      
      
    }
  })
  # ====================================================================================
  
  # ======================== Reactive Data ============================================
  # ------------------------- PCA --------------------------------
  dataInput <- reactive({
    if (is.null(values$check_file)) {
      df <- NULL
      return(df)
    }
    else if (values$check_file == FALSE) {
      if (is.null(input$dataset_pca))
        return(NULL)
      df <- switch(
        input$dataset_pca,
        "Please select" = NULL,
        "rock" = rock,
        "pressure" = pressure,
        "cars" = cars,
        "mtcars" = mtcars
      )
      return(df)
    } else if (values$check_file == TRUE) {
      req(input$file)
      tryCatch({
        df <- read.csv(
          input$file$datapath,
          header = as.logical(input$header),
          sep = input$sep,
          quote = input$quote,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(df)
    }
    else{
      df <- NULL
      return(df)
    }
  })
  pcaObj <- reactive({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      pca_result <-
        pca(
          dataset,
          center = as.logical(input$center),
          scale. = as.logical(input$scale.),
          threshold_percent = input$threshold_percent,
          showall = as.logical(input$showall)
        )
      # print(pca_result)
      return(pca_result)
    }
  })
  pcaObjforPlot <- reactive({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      pca_result <-
        pca(
          dataset,
          center = as.logical(input$center),
          scale. = as.logical(input$scale.),
          threshold_percent = input$threshold_percent,
          showall = TRUE
        )
      return(pca_result)
    }
  })
  # ----------------------- EigenFace --------------------------
  imgInput <- reactive({
    req(input$file_eig)
    tryCatch({
      df <- read.csv(
        input$file_eig$datapath,
        header = as.logical(input$header_eig),
        sep = input$sep,
        quote = input$quote,
        stringsAsFactors = FALSE
      )
      df <- as.matrix(df)
      img <- as.matrix(df[, 2:ncol(df)])
      label <- as.matrix(df[, 1])
      if (!as.logical(input$label_eig)) {
        img <- as.matrix(df[, 1:(ncol(df) - 1)])
        label <- as.matrix(df[, ncol(df)])
      }
      r <- list(df = df,
                img = img,
                label = label)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    return(r)
  })
  eigenFaceforPlot <- reactive({
    if (is.null(imgInput())) {
      return(NULL)
    } else{
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      pca_result <-
        pca_eigenfaces(
          img,
          center = if (is.null(input$center_img))
            TRUE
          else
            as.logical(input$center_img),
          scale. = if (is.null(input$scale._img))
            FALSE
          else
            as.logical(input$scale._img),
          showall = TRUE
        )
      return(pca_result)
    }
  })
  eigenFace <- reactive({
    if (is.null(imgInput())) {
      return(NULL)
    } else{
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      s <- 123
      if (is.null(input$seed_input))
        s <- 123
      else
        s <- input$seed_input
      set.seed(s)
      index = sort(sample(nrow(img), nrow(img) * input$training_percent /
                            100))
      train <- img[index,]
      test <- img[-index,]
      train_label <- label[index,]
      test_label <- label[-index,]
      pca_result <-
        pca_eigenfaces(
          train,
          center = if (is.null(input$center_img))
            TRUE
          else
            as.logical(input$center_img),
          scale. = if (is.null(input$scale._img))
            FALSE
          else
            as.logical(input$scale._img),
          threshold_percent = if (is.null(input$select_threshold_percent_img))
            90
          else
            input$select_threshold_percent_img ,
          showall = FALSE
        )
      pca_result$train <- train
      pca_result$test <- test
      pca_result$index <- index
      pca_result$train_label <- train_label
      pca_result$test_label <- test_label
      pca_result$new_train <-
        data.frame(labels = train_label, data = pca_result$finalData)
      data_new_test <-
        scale(test, center = pca_result$center, scale = pca_result$scale) %*%
        pca_result$eigenfaces
      pca_result$new_test <-
        data.frame(labels = test_label,
                   data = data_new_test)
      return(pca_result)
    }
  })
  # ====================================================================================
  # ======================== UI ============================================
  # ------------------------- Both --------------------------------
  output$setting_tabcard <- renderUI({
    if (input$sidebar_menu == "pca") {
      bs4TabCard(
        id = "setting_tabcard",
        side = "left",
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        
        bs4TabPanel(
          tabName = "Use Dataset",
          active = TRUE,
          fluidRow(column(
            10,
            selectInput(
              inputId = "dataset_pca",
              label = "Choose a dataset:",
              choices = c("Please select", "rock", "pressure", "cars", "mtcars")
            )
          ),
          column(
            2,
            actionBttn(
              inputId = "dataset_btn_pca",
              label = "Process",
              style = "simple",
              color = "success"
            )
          ))
        ),
        bs4TabPanel(
          tabName = "Import File",
          active = FALSE,
          fluidRow(column(
            10,
            fileInput(
              "file",
              "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          column(
            2,
            actionBttn(
              inputId = "file_btn_pca",
              label = "Process",
              style = "simple",
              color = "success"
            )
          )),
          tags$hr(),
          tags$h5("Settings"),
          fluidRow(
            column(
              4,
              radioButtons(
                inputId = 'header',
                label = 'Header',
                choices = c(
                  'This file have headers' = TRUE,
                  'This file does not have headers' = FALSE
                ),
                selected = TRUE
              )
            ),
            column(4, radioButtons(
              'sep',
              'Separator',
              c(
                Comma = ',',
                Semicolon = ';',
                Tab = '\t'
              ),
              ','
            )),
            column(4, radioButtons(
              'quote',
              'Quote',
              c(
                None = '',
                'Double Quote' = '"',
                'Single Quote' = "'"
              ),
              '"'
            ))
          )
        ),
        bs4TabPanel(
          tabName = "PCA Settings",
          active = FALSE,
          p(
            "Select options for the PCA computation (we are using the pca manual function here)"
          ),
          fluidRow(
            column(
              6,
              radioButtons(
                inputId = 'center',
                label = 'Center',
                choices = c(
                  'Shift variables to be zero centered' = TRUE,
                  'Do not shift variables' = FALSE
                ),
                selected = TRUE
              ),
              radioButtons(
                'scale.',
                'Scale',
                choices = c(
                  'Scale variables to have unit variance' = TRUE,
                  'Do not scale variables' = FALSE
                ),
                selected = FALSE
              )
            ),
            column(
              6,
              radioButtons(
                'showall',
                'Select features to obtains new data',
                choices = c(
                  'Show all featurees' = TRUE,
                  'Select the minimum cumulative variance percent of data to obtain the number of features' = FALSE
                ),
                selected = FALSE
              ),
              uiOutput("select_threshold_percent")
            )
          )
        )
      )
    } else if (input$sidebar_menu == "eig_face") {
      bs4TabCard(
        id = "setting_tabcard",
        side = "left",
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        closable = FALSE,
        bs4TabPanel(
          tabName = "Import File",
          active = TRUE,
          fluidRow(column(
            10,
            fileInput(
              "file_eig",
              "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          column(
            2,
            actionBttn(
              inputId = "file_btn_eig",
              label = "Process",
              style = "simple",
              color = "success"
            )
          )),
          tags$hr(),
          tags$h5("Settings"),
          fluidRow(
            column(
              3,
              radioButtons(
                inputId = 'header_eig',
                label = 'Header',
                choices = c(
                  'This file have headers' = TRUE,
                  'This file does not have headers' = FALSE
                ),
                selected = TRUE
              )
            ),
            column(
              3,
              radioButtons(
                inputId = 'label_eig',
                label = 'Label',
                choices = c(
                  'The first column is labels' = TRUE,
                  'The last column is labels' = FALSE
                ),
                selected = FALSE
              )
            ),
            column(3, radioButtons(
              'sep_eig',
              'Separator',
              c(
                Comma = ',',
                Semicolon = ';',
                Tab = '\t'
              ),
              ','
            )),
            column(3, radioButtons(
              'quote_eig',
              'Quote',
              c(
                None = '',
                'Double Quote' = '"',
                'Single Quote' = "'"
              ),
              '"'
            ))
          )
        ),
        bs4TabPanel(
          tabName = "Settings",
          active = FALSE,
          p(
            "Select options for the PCA and classify (we are using the pca manual function here)"
          ),
          fluidRow(
            column(
              6,
              p("PCA"),
              radioButtons(
                inputId = 'center_img',
                label = 'Center',
                choices = c(
                  'Shift variables to be zero centered' = TRUE,
                  'Do not shift variables' = FALSE
                ),
                selected = TRUE
              ),
              radioButtons(
                'scale._img',
                'Scale',
                choices = c(
                  'Scale variables to have unit variance' = TRUE,
                  'Do not scale variables' = FALSE
                ),
                selected = FALSE
              ),
              p(
                'Select the minimum cumulative variance percent of data to obtain the number of features'
              ),
              sliderInput(
                "select_threshold_percent_img",
                "Minimum Percentage:",
                min = 1,
                max = 100,
                value = 90
              )
            ),
            column(
              6,
              p("Classify"),
              sliderInput(
                "seed_input",
                "Seed:",
                min = 1,
                max = 1000,
                value = 123
              ),
              sliderInput(
                "training_percent",
                "Training Percentage:",
                min = 1,
                max = 99,
                value = 80
              )
            )
          )
        )
      )
    }
  })
  getPage<-function() {
      return(includeHTML("./R/ui/html/report.html"))
  }
  output$doc<-renderUI({getPage()})
  # ------------------------- PCA --------------------------------
  output$select_pc_plot <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      fluidRow(
        column(12, p("Select the PCs to plot")),
        column(4, uiOutput("pcs_plot_x")),
        column(4,  uiOutput("pcs_plot_y")),
        column(4, uiOutput("grouping_var")),
        column(12, tags$hr())
      )
    }
  })
  output$grouping_var <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    }
    else{
      dataset <- dataInput()
      p("Select the grouping variable.")
      selectInput(
        inputId = "grouping_var",
        label = "Grouping variable:",
        choices = c("None", names(dataset))
      )
    }
  })
  output$pcs_plot_x <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      data <- pcaObject$finalData
      selectInput(
        inputId = "pcs_plot_x",
        label = "X axis:",
        choices = colnames(data),
        selected = 'PC1'
      )
    }
  })
  output$pcs_plot_y <- renderUI({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      data <- pcaObject$finalData
      d <- colnames(data)
      c <- d[-match(input$pcs_plot_x, colnames(data))]
      # drop down selection
      selectInput(
        inputId = "pcs_plot_y",
        label = "Y axis:",
        choices = c,
        selected = 'PC2'
      )
    }
    
  })
  # Check boxes to choose columns
  output$choose_columns_biplot <- renderUI({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      data <- dataInput()
      colnames <- names(data)
      fluidRow(# Create the checkboxes and select them all by default
        column(
          12,
          checkboxGroupInput(
            "columns_biplot",
            "Choose up to five columns to display on the scatterplot matrix",
            choices  = colnames,
            selected = colnames[1:5]
          )
        ), column(12, tags$hr()))
      
    }
  })
  output$select_threshold_percent <- renderUI({
    if (input$showall == FALSE) {
      sliderInput(
        "threshold_percent",
        "Minimum Percentage:",
        min = 1,
        max = 100,
        value = 90
      )
      
    }
  })
  # ----------------------- EigenFace --------------------------
  output$previous_1 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      actionBttn(
        inputId = "previous_1",
        label = "Previous",
        style = "simple",
        color = "primary"
      )
    }
  })
  output$next_1 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      actionBttn(
        inputId = "next_1",
        label = "Next",
        style = "simple",
        color = "primary"
      )
    }
  })
  output$previous_2 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        actionBttn(
          inputId = "previous_2",
          label = "Previous",
          style = "simple",
          color = "primary"
        )
      }
    }
  })
  output$next_2 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        actionBttn(
          inputId = "next_2",
          label = "Next",
          style = "simple",
          color = "primary"
        )
      }
    }
  })
  output$previous_3 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        actionBttn(
          inputId = "previous_3",
          label = "Previous",
          style = "simple",
          color = "primary"
        )
      }
    }
  })
  output$next_3 <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        actionBttn(
          inputId = "next_3",
          label = "Next",
          style = "simple",
          color = "primary"
        )
      }
    }
  })
  output$select_pic_no_train <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObj <- eigenFace()
        index <- as.vector(pcaObj$index)
        # print(index)
        fluidRow(column(12, p("Please select picture no.#")),
                 column(
                   12,
                   selectInput(
                     inputId = "select_pic_no_train",
                     label = "Picture No.#:",
                     choices = index
                   )
                 ),
                 column(12, tags$hr()))
      }
    }
  })
  output$select_pic_no_test <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      if (is.null(eigenFace()) == FALSE) {
        pcaObj <- eigenFace()
        index <- as.vector(pcaObj$index)
        no_test <- c(1:nrow(img))
        no_test <- no_test[-index]
        # print(index)
        fluidRow(column(12, p(
          "Please select picture no.# to recognize"
        )),
        column(
          12,
          selectInput(
            inputId = "select_pic_no_test",
            label = "Picture No.#:",
            choices = no_test
          )
        ),
        column(12, tags$hr()))
      }
    }
  })
  output$n_fold <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      no_fold <- which(nrow(img) %% 1:20 == 0)[-1]
      fluidRow(column(12, p(
        "Please number of fold for n-fold cross validation."
      )),
      column(
        12,
        selectInput(
          inputId = "n_fold",
          label = "N-fold:",
          choices = no_fold
        )
      ))
      
    }
  })
  output$n_repetition <- renderUI({
    if (is.null(imgInput()) == FALSE) {
      fluidRow(column(12, p(
        "Please number of Repetition for evaluation."
      )),
      column(
        12,
        selectInput(
          inputId = "n_repetition",
          label = "Number of repetition:",
          choices = 1:10
        )
      ))
    }
  })
  # ====================================================================================
  
  # ======================== Output ============================================
  # ------------------------- PCA --------------------------------
  output$summary_verbatim <- renderPrint({
    if (is.null(dataInput())) {
      return(invisible())
    } else{
      dataset <- dataInput()
      return(summary(dataset))
    }
  })
  output$new_data_pca <- renderDataTable({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      new_data <- pcaObj()
      return(new_data$finalData)
    }
  }, style = "bootstrap4")
  output$new_data_plot <- renderPlot({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      pcaObject <- pcaObj()
      data <- pcaObject$finalData
      # print(data)
      columns_biplot <- paste0("PC", seq_len(ncol(data)))
      new_data_subset_biplot <- data[, columns_biplot, drop = FALSE]
      ggpairs(new_data_subset_biplot)
    }
  })
  output$summary_pca <- renderDataTable({
    if (is.null(pcaObj())) {
      return(NULL)
    } else{
      pcaObject <- pcaObj()
      return(format(
        pcaObject$summary,
        nsmall = 2,
        digits = 3,
        scientific = FALSE
      ))
    }
  }, style = "bootstrap4")
  # display a summary of the CSV contents
  output$summary_plot <-  renderDataTable({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      dataset <- dataInput()
      print_data <-
        format(
          psych::describe(dataset),
          nsmall = 2,
          digits = 3,
          scientific = FALSE
        )
      return(t(print_data))
    }
  }, style = "bootstrap4")
  output$contents <- renderDataTable({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      return(dataInput())
    }
  }, style = "bootstrap4")
  # corr plot
  output$correlation <- renderPlot({
    if (is.null(dataInput())) {
      return(NULL)
    } else{
      data <- dataInput()
      # Keep the selected columns
      columns_biplot <-    input$columns_biplot
      # print(columns_biplot)
      # print(colnames(data))
      if (is.null(input$columns_biplot)) {
        return(NULL)
      }
      else{
        data_subset_biplot <- data[, input$columns_biplot, drop = FALSE]
        return(ggpairs(data_subset_biplot))
      }
    }
    
  })
  output$var_plot <- renderPlot({
    if (is.null(dataInput())) {
      return(NULL)
    }
    else{
      dataset <- dataInput()
      if (is.null(pcaObjforPlot())) {
        return(NULL)
      } else{
        pcaObject <- pcaObjforPlot()
        cumvar <- paste(round(pcaObject$summary.percent, 1), "%")
        
        eig_df <- data.frame(cumvar)
        PCs <- colnames(pcaObject$finalData)
        
        # print(PCs)
        ggplot(eig_df, aes(
          reorder(PCs, -pcaObject$eigenvalues),
          pcaObject$eigenvalues
        )) +
          geom_bar(stat = "identity",
                   fill = "white",
                   colour = "black") +
          geom_text(label = cumvar,
                    size = 4,
                    vjust = -0.4) +
          theme_bw(base_size = 14) +
          xlab("Principal Components") +
          ylab("Variances") +
          ylim(0, (max(pcaObject$eigenvalues) * 1.1))
      }
      
    }
  })
  output$pc_plot <- renderPlot({
    if (is.null(pcaObjforPlot())) {
      return(NULL)
    } else{
      pcaObject <- pcaObjforPlot()
      if (is.null(input$pcs_plot_x) ||
          is.null(input$pcs_plot_y)) {
        return(NULL)
      } else{
        x <- match(input$pcs_plot_x, colnames(pcaObject$finalData))
        x <- as.numeric(x)
        y <- match(input$pcs_plot_y, colnames(pcaObject$finalData))
        y <- as.numeric(y)
        if (is.null(input$grouping_var) ||
            input$grouping_var == "None") {
          if (is.null(dataInput())) {
            return(NULL)
          } else{
            dataset <- dataInput()
            data <- na.omit(dataset)
            if (x != y) {
              autoplot_pca(
                pcaObject,
                data = data,
                loadings = TRUE,
                loadings.colour = 'blue',
                loadings.label = TRUE,
                loadings.label.size = 3,
                x = x,
                y = y
              )
            } else{
              return(NULL)
            }
            
          }
        }
        else{
          if (is.null(dataInput())) {
            return(NULL)
          } else{
            dataset <- dataInput()
            data <- na.omit(dataset)
            grouping <- input$grouping_var
            if (x != y) {
              autoplot_pca(
                pcaObject,
                data = data,
                colour = grouping,
                loadings = TRUE,
                loadings.colour = 'blue',
                loadings.label = TRUE,
                loadings.label.size = 3,
                x = x,
                y = y
              )
            } else{
              return(NULL)
            }
          }
        }
      }
    }
  })
  # ----------------------- EigenFace --------------------------
  output$pages1 <- renderText({
    if (is.null(imgInput()) == FALSE) {
      df <- imgInput()
      df <- df$df
      max_page <- as.numeric(ceiling(nrow(df) / 100))
      
      
      paste(index1(), "of", max_page, sep = "  ")
    }
  })
  output$pages2 <- renderText({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        pca_img <- pcaObject$eigenfaces
        max_page <-
          as.numeric(ceiling(ncol(pcaObject$eigenfaces) / 100))
        
        
        paste(index2(), "of", max_page, sep = "  ")
      }
    }
  })
  output$pages3 <- renderText({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        pca_img <- pcaObject$eigenfaces
        max_page <-
          as.numeric(ceiling(ncol(pcaObject$eigenfaces) / 100))
        
        paste(index3(), "of", max_page, sep = "  ")
      }
    }
  })
  output$imgInputData <- renderImage({
    if (is.null(imgInput())) {
      return(NULL)
    } else{
      df <- imgInput()
      img <- df$img
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.png')
      
      # Generate the PNG
      png(outfile, width = 900, height = 400)
      # hist(rnorm(1000), main = "Generated in renderImage()")
      ini <-  (1 + (100 * (index1() - 1)))
      end <- (100 * index1())
      par(mfrow = c(5, 20))
      par(mar = c(0.2, 0.2, 0.2, 0.2))
      for (i in ini:end) {
        if (i <= nrow(img)) {
          show_img(t(apply(
            matrix(
              as.numeric(img[i, 1:ncol(img)]),
              nrow = sqrt(ncol(img)),
              byrow = T
            ), 2, rev
          )))
        }
        # show_img(apply(matrix(as.numeric(df[i,1:4096]), nrow=64, byrow=T), 2, rev))
        # image(matrix(df[i,1:4096], nrow =64,byrow =F), col = grey(seq(0, 1, length = 256)))
      }
      dev.off()
      
      # Return a list containing the filename
      list(
        src = outfile,
        contentType = 'image/png',
        width = 900,
        height = 400,
        alt = "This is alternate text"
      )
    }
  }, deleteFile = TRUE)
  output$cumvar_img_plot <- renderPlot({
    if (is.null(imgInput()) == FALSE) {
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      if (is.null(eigenFaceforPlot()) == FALSE) {
        pcaObject <- eigenFaceforPlot()
        cumvar <- round(pcaObject$summary.percent, 1)
        return(
          plot(
            cumvar,
            xlab = "The number of eigenfaces",
            ylab = "Cumulative Proportion of variance explained (% percentage)",
            ylim = c(0, 100),
            type = 'b'
          )
        )
      }
    }
    
  })
  output$imgOutputTraining <- renderImage({
    if (is.null(imgInput())) {
      return(NULL)
    } else{
      r <- imgInput()
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      if (is.null(eigenFace())) {
        return(NULL)
      } else{
        pcaObject <- eigenFace()
        finalData <- as.matrix(pcaObject$finalData)
        eigenfaces <- as.matrix(pcaObject$eigenfaces)
        averagefaces <- as.matrix(pcaObject$averagefaces)
        # Reconstruction of the photo from the eigenvector space
        pca_img <- as.matrix(finalData %*% t(eigenfaces))
        # print(pca_img)
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, width = 900, height = 400)
        # hist(rnorm(1000), main = "Generated in renderImage()")
        ini <-  (1 + (100 * (index3() - 1)))
        end <- (100 * index3())
        par(mfrow = c(5, 20))
        par(mar = c(0.2, 0.2, 0.2, 0.2))
        # print("ncol pca_img")
        # print(ncol(pca_img))
        # print(avf)
        for (i in ini:end) {
          if (i <= nrow(pca_img)) {
            # add the average face
            show_img(t(apply(
              matrix(
                as.numeric(pca_img[i,] + averagefaces),
                nrow = sqrt(ncol(img)),
                ncol = sqrt(ncol(img)),
                byrow = T
              ),
              2,
              rev
            )))
          }
        }
        dev.off()
        
        # Return a list containing the filename
        list(
          src = outfile,
          contentType = 'image/png',
          width = 900,
          height = 400,
          alt = "This is alternate text"
        )
        
      }
    }
  }, deleteFile = TRUE)
  output$avg_face <- renderImage({
    if (is.null(imgInput())) {
      return(NULL)
    } else{
      r <- imgInput()
      img <- r$img
      
      if (is.null(eigenFace())) {
        return(NULL)
      } else{
        pcaObject <- eigenFace()
        avf <- pcaObject$center

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, width = 300, height = 400)
        show_img(t(apply(
          matrix(
            as.numeric(avf),
            nrow = sqrt(ncol(img)),
            byrow = T
          ), 2, rev
        )))
        
        
        dev.off()
        
        # Return a list containing the filename
        list(
          src = outfile,
          contentType = 'image/png',
          width = 300,
          height = 400,
          alt = "This is alternate text"
        )
        
        
      }
    }
  }, deleteFile = TRUE)
  output$imgOutputEig <- renderImage({
    if (is.null(imgInput()) == FALSE) {
      r <- imgInput()
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        pca_img <- pcaObject$eigenfaces
        # print(pca_img)
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, width = 900, height = 400)
        # hist(rnorm(1000), main = "Generated in renderImage()")
        ini <-  (1 + (100 * (index2() - 1)))
        end <- (100 * index2())
        par(mfrow = c(5, 20))
        par(mar = c(0.2, 0.2, 0.2, 0.2))
        for (i in ini:end) {
          if (i <= ncol(pca_img)) {
            show_img(t(apply(
              matrix(
                as.numeric(pca_img[, i]),
                nrow = sqrt(ncol(img)),
                ncol = sqrt(ncol(img)),
                byrow = T
              ),
              2,
              rev
            )))
          }
        }
        dev.off()
        
        # Return a list containing the filename
        list(
          src = outfile,
          contentType = 'image/png',
          width = 900,
          height = 400,
          alt = "This is alternate text"
        )
        
      }
    }
  }, deleteFile = TRUE)
  output$projection <- renderPlot({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObj <- eigenFace()
        index <- as.vector(pcaObj$index)
        df <- pcaObj$finalData
        print(pcaObj$index)
        selectd <- c(as.numeric(input$select_pic_no_train))
        print(selectd)
        pic_no <- match(selectd, as.vector(index))
        pic_no <- as.numeric(pic_no)
        if (is.null(input$select_pic_no_train) == FALSE) {
          barplot(
            as.matrix(df[pic_no,]),
            main = "projection coefficients in eigen space",
            col = "blue",
            axisnames = FALSE
          )
        }
        
      }
    }
  })
  output$classify_result <- renderDataTable({
    if (is.null(imgInput()) == FALSE) {
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        type = c("Euclidean", "Mahalanobis", "Manhattan")
        accuracy <- vector("numeric", length(type))
        for (i in c(1:3)) {
          accuracy[i] <-
            classify(pcaObject$new_train,
                     pcaObject$new_test,
                     i,
                     pcaObject$eigenvalues) * 100
        }
        result <- data.frame(type = type, accuracy = accuracy)
      }
    }
  })
  output$recognize_result <- renderDataTable({
    if (is.null(imgInput()) == FALSE) {
      r <- imgInput()
      df <- r$df
      img <- r$img
      label <- r$label
      if (is.null(eigenFace()) == FALSE) {
        pcaObject <- eigenFace()
        test_label <- pcaObject$test_label
        index <- as.vector(pcaObject$index)
        no_test <- c(1:nrow(img))
        no_test <- no_test[-index]
        pred <- list()
        
        pred$Euclidean <-
          recognize(pcaObject$new_train,
                    pcaObject$new_test,
                    1,
                    pcaObject$eigenvalues)
        pred$Mahalanobis <-
          recognize(pcaObject$new_train,
                    pcaObject$new_test,
                    2,
                    pcaObject$eigenvalues)
        pred$Manhattan <-
          recognize(pcaObject$new_train,
                    pcaObject$new_test,
                    3,
                    pcaObject$eigenvalues)
        result <- NULL
        if (is.null(input$select_pic_no_test) == FALSE) {
          selectd <- c(as.numeric(input$select_pic_no_test))
          pic_no <- match(selectd, as.vector(no_test))
          pic_no <- as.numeric(pic_no)
          print(pic_no)
          labels <-
            c(test_label[pic_no], test_label[pic_no], test_label[pic_no])
          type = c("Euclidean", "Mahalanobis", "Manhattan")
          prediction <-
            c(pred$Euclidean[pic_no],
              pred$Mahalanobis[pic_no],
              pred$Manhattan[pic_no])
          compare <- labels == prediction
          result <-
            data.frame(
              type = type,
              labels = labels,
              prediction = prediction,
              compare = compare
            )
          
        }
        return(result)
      }
    }
  })
  output$eva <- renderDataTable({
    if (is.null(imgInput()) == FALSE) {
      data <- imgInput()
      img <- data$img
      if (!is.null(input$n_fold) &&
          !is.null(input$n_repetition) &&
          !is.null(input$seed_input) &&
          !is.null(input$center_img) &&
          !is.null(input$scale._img) &&
          !is.null(input$select_threshold_percent_img)) {
        accuracy <-
          n_fold_cross_validation(
            data = data,
            n_fold = as.numeric(input$n_fold),
            n_repetition = as.numeric(input$n_repetition),
            seed_input = as.numeric(input$seed_input),
            center = as.logical(input$center_img),
            scale. = as.logical(input$scale._img),
            threshold = as.numeric(input$select_threshold_percent_img)
          )
        return(accuracy)
      } else{
        accuracy <-
          n_fold_cross_validation(
            data = data,
            n_fold = min(which(nrow(img) %% 1:20 == 0)[-1]),
            n_repetition = 1,
            seed_input = 123,
            center = TRUE,
            scale. = FALSE,
            threshold = 90
          )
        return(accuracy)
      }
    }
  })
  # ====================================================================================
  
  # ======================== Function ============================================
  # ------------------------- EigenFace --------------------------------
  show_img <- function(x) {
    image(x, col = grey(seq(0, 1, length = 256)))
  }
}
