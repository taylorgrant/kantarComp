library(shiny)

kantarApp <- function(...) {

  ui = fluidPage(
    titlePanel("Kantar Creative Pull"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose xlsx file',
                  accept = c(".xlsx", ".xls")
        ),
        htmlOutput("table") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
        downloadButton("report", "Generate Report")
      ),
      mainPanel(
        # htmlOutput("about")
      )
    )
  )

  server = function(input, output, session) {

    # output$about <- renderUI({HTML("<b>What is this?</b><br>This is an app that designed to ingest and process Kantar competitive
    #                                data. The app will read in all available spend data for each advertiser, and will also scan through
    #                                and render all digital advertising creative in the form of gifs and imagery.<br><br>
    #                                <b>What do I need to do?</b><br>Use the browse button at left to find the Kantar file that you want to
    #                                upload an ingest. The file should be an <b>'.xlsx'</b> and it should begin at the <b>Parent</b> level
    #                                within Kantar's hierarchy.<br><br>
    #                                <b>Anything else I should know?</b><br> This app can take several minutes depending how much creative is available.
    #                                Each image is read in individually. If an advertiser is using a gif, then each image of the gif must be read in.
    #                                <em>Please be patient.</em><br><br> When the app is finished, it will display a table with the number of assets by format type
    #                                above the<code>Generate Report</code> button. Once you see the table, click the Report button and an HTML file will process and be downloaded to your
    #                                computer.")})

    # now that i have the data it has to be processed

    data_out <- reactive({
      req(input$file1)
      inFile <- input$file1
      pull_kantar(inFile$datapath)
    })

    # output$table <- renderTable({
    #   image_out() %>%
    #     distinct(creative, .keep_all = TRUE) %>%
    #     count(Format = format) %>%
    #     rename(Total = n) %>%
    #     filter(!is.na(Format))
    #     # knitr::kable("html", caption = "Imagery Summary") %>%
    #     # kable_styling(full_width = T)
    # })

    output$table <- renderText({
      data_out()$image_out %>%
        dplyr::distinct(creative, .keep_all = TRUE) %>%
        dplyr::count(Format = format) %>%
        dplyr::rename(Total = n) %>%
        dplyr::filter(!is.na(Format)) %>%
        knitr::kable("html", caption = "Imagery Summary") %>%
        kableExtra::kable_styling(full_width = T)
    })

    # spend_data <- reactive({
    #   summarise_spend(image_out())
    # })

    # pull_kantar(input$file1)
    # output$hcontainer <- renderHighchart({
    #
    #   spend_data <- summarise_spend(kantar_data())
    #   hcoptslang <- getOption("highcharter.lang")
    #   hcoptslang$thousandsSep <- ","
    #   options(highcharter.lang = hcoptslang)
    #
    #   pal <- c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce',
    #            '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a')
    #
    #   hc <- hchart(spend_data$advertiser, 'column', hcaes(x = quarter, y = spend, group = advertiser, marker = FALSE)) %>%
    #     hc_xAxis(title = list(text = "Quarterly Advertiser Spend"),
    #              type = "category") %>%
    #     hc_yAxis(title = list(text = "Spend")) %>%
    #     hc_colors(pal) %>%
    #     hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
    #     hc_title(text = paste0("Spend by Quarter")) %>%
    #     hc_credits(enabled = TRUE,
    #                text = "Source: ",
    #                style = list(fontSize = "10px"))
    #
    #
    #
    # })


    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # system.file("rmd", "report.Rmd", package="kantarComp")
        # tempReport <- file.path(tempdir(), "report.Rmd")
        tempReport <- system.file("rmd", "report.Rmd", package="kantarComp")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(data = data_out()
                       # filename = stringr::str_replace_all(file()$name, ".xlsx", ".rds")
        )

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
