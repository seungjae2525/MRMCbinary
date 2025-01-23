#' @title Multi-reader multi-case dataset
#'
#' @format A data frame with 1140 rows and 7 variables:
#' \describe{
#'   \item{reader}{Reader identifier}
#'   \item{treatment}{Treatment (i.e., modality) identifier}
#'   \item{case}{Case identifier (factorial design)}
#'   \item{case2}{Case identifier (cases nested within readers)}
#'   \item{case3}{Case identifier (cases nested within treatments)}
#'   \item{truth}{True disease status (1 = positive, 0 = negative)}
#'   \item{rating}{Ordinal reader ratings of case status (i.e., diagnostic test result)}
#' }
#'
#' @details One can directly use this data from MRMCaov package.
#'
#' @references
#' Van Dyke, C. W., White, R. D., Obuchowski, N. A., Geisinger, M. A., Lorig, R. J., & Meziane, M. A. (1993). Cine MRI in the diagnosis of thoracic aortic dissection. 79th RSNA Meetings. *Chicago, IL*, 28.
#'
#' @source <https://github.com/brian-j-smith/MRMCaov/tree/master/data>
#'
#' @keywords datasets
"VanDyke"
