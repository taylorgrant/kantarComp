library(shiny)

kantarApp <- function(...) {

  ui = shiny::fluidPage(
    shiny::titlePanel("Kantar Creative Pull"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("slider1", label = "Assets per advertiser", min = 0,
                    max = 100, value = 40),
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

    shiny::showModal(shiny::modalDialog(shiny::HTML("<p style='color:red;'><b>Important:</b> Please open the file downloaded from Kantar and resave it.
                                                    The Kantar excel download is missing a necessary XML file. After you resave the file, it can be read in.</p><hr>
                                                    <b>What is this?</b><br>This is an app that will ingest and process Kantar competitive
                                   data. The app will read in all available spend data for each advertiser, and will also scan through
                                   and render up to 100 digital banners or gifs per advertiser that ran during the previous quarter.<br><br>
                                   <b>What do I need to do?</b><br>First, select the maximum number of assets that should be rendered in the report. The assets
                                   are sorted by spend, so whatever you choose for the slider value, the report will pull the N assets with the greatest
                                   spend during the last quarter.<br><br>
                                   Next, use the browse button at left to find the Kantar file that you want to
                                   upload. The file should be an excel doc - either <b>'.xlsx'</b> or <b>'.xls'</b>. Once read in and processed,
                                   total quarterly spend data will be calculated at multiple levels -- advertiser, brand, product, and media.<br><br>
                                   <b>Anything else I should know?</b><br> This app can take upwards of 10 minutes to run depending on the number of creative assets.
                                   <em>Please be patient.</em><br><br> When the data has been processed, graphs and tables will appear in the main panel. A table with the
                                   total counts of image assets will also appear at left above the <code>Generate Report</code> button. Once you see the table,
                                   click <code>Generate Report</code> and an HTML file will process, which you can save to your computer.<br><br>
                                   Two more notes - first, Kantar can sometimes misattribute creative assets to advertisers; second, there is no way of de-duplicating
                                   creative assets. It's not out of the ordinary for the same creative to be assigned multiple unique ids."),
                               easyClose = T))

    # now that i have the data it has to be processed

      data_out <- shiny::reactive({
        shiny::req(input$file1, input$slider1)
        inFile <- input$file1
        pull_kantar(inFile$datapath, input$slider1)
      })


    # summary image table
    output$table <- shiny::renderText({
      data_out()$image_out %>%
        dplyr::distinct(creative, .keep_all = TRUE) %>%
        dplyr::count(Format = format) %>%
        dplyr::rename(Total = n) %>%
        dplyr::filter(!is.na(Format)) %>%
        dplyr::rename(Format = Format) %>%
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

    brandcount <- shiny::reactive({
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
        unlink(c("kantar_jpgs", "tmpdata", "kantar_gifs"), recursive=TRUE)
      }
    )

  }

  shiny::shinyApp(ui, server)

}
