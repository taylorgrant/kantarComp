library(shiny)

kantarApp <- function(...) {

  ui = shiny::fluidPage(
    shiny::titlePanel("Kantar Creative Pull"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput('file1', 'Choose xlsx file',
                  accept = c(".xlsx", ".xls")
        ),
        shiny::htmlOutput("table") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
        shiny::downloadButton("report", "Generate Report")
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          title = "Plots",
          shiny::column(width = 11,
                 shiny::tabsetPanel(
                   shiny::tabPanel("Advertiser",
                                   highcharter::highchartOutput("advertiser", height = "450px")
                                   ),
                   shiny::tabPanel("Brand",
                                   highcharter::highchartOutput("brand", height = "450px")
                                   ),
                   shiny::tabPanel("Product",
                                   highcharter::highchartOutput("product", height = "450px")
                                   ),
                   shiny::tabPanel("Media",
                                   highcharter::highchartOutput("media", height = "450px")
                   )
                   )
                 )
          ),
        shiny::fluidRow(
          title = "Tables",
          shiny::column(width = 11,
                 shiny::tabsetPanel(
                   shiny::tabPanel("Advertiser",
                                   DT::dataTableOutput("advertiser_tbl")
                                   ),
                   shiny::tabPanel("Brand",
                                   DT::dataTableOutput("brand_tbl")
                                   ),
                   shiny::tabPanel("Product",
                                   DT::dataTableOutput("product_tbl")
                                   ),
                   shiny::tabPanel("Media",
                                   DT::dataTableOutput("media_tbl")
                   )
                   )
                 )
          )
        )
      )
    )


  server = function(input, output, session) {

    shiny::showModal(shiny::modalDialog(shiny::HTML("<b>What is this?</b><br>This is an app that will ingest and process Kantar competitive
                                   data. The app will read in all available spend data for each advertiser, and will also scan through
                                   and render any digital banners or gifs that Kantar has identified.<br><br>
                                   <b>What do I need to do?</b><br>Use the browse button at left to find the Kantar file that you want to
                                   upload. The file should be an excel doc - either <b>'.xlsx'</b> or <b>'.xls'</b>. Once read in and processed,
                                   total quarterly spend data will be calculated at the advertiser, brand, product, and media levels.<br><br>
                                   <b>Anything else I should know?</b><br> This app can take several minutes depending on the number of creative assets.
                                   Each image is read in individually. If an advertiser is using a gif, then each image of the gif must be read in as well.
                                   <em>Please be patient.</em><br><br> When the data has been processed, graphs and tables will appear in the main panel. A table with the
                                   total counts of image assets will also appear at left above the <code>Generate Report</code> button. Once you see the table,
                                   click <code>Generate Report</code> and an HTML file will process, which you can save to your computer.<br><br>
                                   One other thing to note is that sometimes Kantar will mess up and misidentify the ads running for a certain advertiser."),
                               easyClose = T))

    # now that i have the data it has to be processed

    data_out <- reactive({
      req(input$file1)
      inFile <- input$file1
      pull_kantar(inFile$datapath)
    })

    # summary image table
    output$table <- renderText({
      data_out()$image_out %>%
        dplyr::distinct(creative, .keep_all = TRUE) %>%
        dplyr::count(Format = format) %>%
        dplyr::rename(Total = n) %>%
        dplyr::filter(!is.na(Format)) %>%
        knitr::kable("html", caption = "Imagery Summary") %>%
        kableExtra::kable_styling(full_width = T)
    })


    # highcharter spend data
    output$advertiser <- highcharter::renderHighchart({
      plot_highchart(data_out()$spend, advertiser)
    })
    output$brand <- highcharter::renderHighchart({
      plot_highchart(data_out()$spend, brand)
    })
    output$product <- highcharter::renderHighchart({
      plot_multichart(data_out()$spend, brand, product)
    })
    output$media <- highcharter::renderHighchart({
      plot_multichart(data_out()$spend, brand, media)
    })

    # tables of spend data
    output$advertiser_tbl <- DT::renderDT({
      spend_table(data_out()$spend, "advertiser")
    })
    output$brand_tbl <- DT::renderDT({
      spend_table(data_out()$spend, "brand")
    })
    output$product_tbl <- DT::renderDT({
      spend_table(data_out()$spend, "product")
    })
    output$media_tbl <- DT::renderDT({
      spend_table(data_out()$spend, "media")
    })

    brandcount <- reactive({
      length(unique(data_out()$image_out$advertiser))
    })

    # download handler for rmarkdown
    output$report <-shiny::downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- system.file("rmd", switch(brandcount(),
                                                "report.Rmd", "report2.Rmd", "report3.Rmd",
                                                "report4.Rmd", "report5.Rmd"),
                                  package="kantarComp")
        file.copy(switch(brandcount(), "report.Rmd", "report2.Rmd", "report3.Rmd",
                         "report4.Rmd", "report5.Rmd"), tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(data = data_out())

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  }

  shiny::shinyApp(ui, server)

}
