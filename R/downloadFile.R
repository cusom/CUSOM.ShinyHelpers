#' Utility module to assist in dataset downloads
#'
#' @param id namespace for module
#' @param fileName - string - name of file for download
#' @param dataForDownload - tibble or dataframe - data to download
#' @param downloadButtonLabel - string - label for download button
#' @return module to handle file download - ui elements and server componenents
#' @import shiny
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom vroom vroom_write
#' @importFrom shinyalert shinyalert
#' @importFrom writexl write_xlsx
#' @export
downloadFile <- function(id, fileName, dataForDownload, downloadButtonLabel = "Download") {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny::showModal(
      htmltools::tags$div(
        id = ns("DataDownloadAlert"),
        shiny::modalDialog(
          title = htmltools::tags$h3(glue::glue("Download options:")),
          size = "m",
          easyClose = TRUE,
          list(
            htmltools::tags$div(
              style = "text-align:left",
              shinyWidgets::prettyRadioButtons(
                inputId = ns("downloadFileType"),
                label = "Choose file type:",
                choices = downloadFile_AvailableTypes(),
                status = "success"
              )
            )
          ),
          footer = shiny::tagList(
            shiny::downloadButton(
              outputId = ns("download"),
              label = downloadButtonLabel,
              icon = shiny::icon("download"),
              style = "float:left;"
            ),
            shiny::modalButton(label = "Cancel")
          )
        )
      )
    )

    output$download  <- shiny::downloadHandler(

      filename = function() {
        glue::glue("{fileName}.{input$downloadFileType}")
      },

      content = function(file) {
        # To Do: refactor here

        if (input$downloadFileType == "csv") {

          shinybusy::show_modal_spinner(
            spin = "hollow-dots",
            color = "#3c8dbc",
            text = "Preparing download..."
          )
          on.exit(shinybusy::remove_modal_spinner())
          vroom::vroom_write(dataForDownload, file, delim = ",")
        }
        if (input$downloadFileType == "tsv") {
          shinybusy::show_modal_spinner(
            spin = "hollow-dots",
            color = "#3c8dbc",
            text = "Preparing download..."
          )
          on.exit(shinybusy::remove_modal_spinner())
          vroom::vroom_write(dataForDownload, file, delim = "\t")
        }
        if (input$downloadFileType == "tsv.gz") {
          shinybusy::show_modal_spinner(
            spin = "hollow-dots",
            color = "#3c8dbc",
            text = "Preparing download..."
          )
          on.exit(shinybusy::remove_modal_spinner())
          vroom::vroom_write(dataForDownload, file, delim = "\t")
        }
        if (input$downloadFileType == "xlsx"){
          shinybusy::show_modal_spinner(
            spin = "hollow-dots",
            color = "#3c8dbc",
            text = "Preparing download..."
          )
          on.exit(shinybusy::remove_modal_spinner())
          writexl::write_xlsx(dataForDownload,file)
        }

        shinyalert::shinyalert(
          title = "Success!",
          html = TRUE,
          text = shiny::tagList(
            htmltools::tags$p(
              glue::glue("{fileName}.{input$downloadFileType}")
            ),
            htmltools::tags$p("Download complete")
          ),
          type = "success",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showCancelButton = FALSE,
          timer = 1000
        )

      }

    )
  })

}

downloadFile_AvailableTypes <- function() {
  return(
    tibble::tibble(
      fileType = c("Comma-Delimited", "Tab-Delimited",
        "G-Zipped Tab-Delimited", "Excel"),
      extension = c("csv", "tsv", "tsv.gz", "xlsx")
    ) |>
      tibble::deframe()
  )
}
