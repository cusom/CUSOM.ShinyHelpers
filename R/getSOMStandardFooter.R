
#' Utility function to get standard School of Medicine Footer HTML
#'
#' @param footerImageFilePath optional parameter to source a specfic image for footer. Assumes image is in "www" directroy in application folder
#' @param additionalText string - optional text to include below footer image
#' @return HTML
#' @export

getSOMStandardFooter <- function(footerImageFilePath='./images/medicine_h_clr.png', additionalText ='') {

  return(
    paste0(
      '<br />
      <hr />
        <center>
          <img src="',footerImageFilePath,'" width="400" height="auto">
        </center>
      <br />
      <p>',additionalText,'</p>
      <hr />
      <div class="u-foottextwrap">
        <p>&copy; ',format(Sys.Date(), "%Y"),'&nbsp;
          <a href="http://www.cu.edu/regents/">
          <strong> The Regents of the University of Colorado,</strong>
          </a>a body corporate. All rights reserved.
        </p>
        <p> Accredited by the
          <a href=\"https://www.hlcommission.org/component/directory/?Action=ShowBasic&amp;Itemid=&amp;instid=1040\">
          <strong> Higher Learning Commission.</strong>
          </a>All trademarks are registered property of the University. Used by permission only.
        </p>
      </div>
      '
    )
  )

}

