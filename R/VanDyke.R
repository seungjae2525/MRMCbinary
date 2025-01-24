#' @title Multi-reader multi-case dataset
#'
#' @description Example data from a study comparing the relative performance of cinematic presentation of magnetic resonance imaging (CINE MRI) to single spin-echo magnetic resonance imaging (SE MRI) for the detection of thoracic aortic dissection (Van Dyke et al., 1993).
#'
#' @format A data frame with 1140 rows and 7 variables:
#' \describe{
#'   \item{reader}{Reader identifier for the five radiologists}
#'   \item{treatment}{Treatment identifier for the two imaging modalities}
#'   \item{case}{Case identifiers for 114 cases}
#'   \item{case2}{Case identifier (cases nested within readers)}
#'   \item{case3}{Case identifier (cases nested within treatments)}
#'   \item{truth}{
#'     Indicator for thoracic aortic dissection (i.e., true disease status):
#'     1 = performed (i.e., patients with aortic dissection imaged with both SE MRI and CINE MRI) or
#'     0 = not performed (i.e., patients without dissection imaged with both SE MRI and CINE MRI)
#'   }
#'   \item{rating}{
#'     Five-point ratings given to case images by the radiologists (i.e., diagnostic test result):
#'     1 = definitely no aortic dissection, 2 = probably no aortic dissection, 3 = unsure about aortic dissection, 4 = probably aortic dissection, or 5 = definitely aortic dissection
#'   }
#' }
#'
#' @details This example compares the relative performance of SE MRI with the CINE MRI in detecting thoracic aortic dissection.
#' There are 45 patients with an aortic dissection and 69 patients without a dissection imaged with both SE MRI and CINE MRI.
#' One can directly use this data from \code{MRMCaov} package. See \bold{Source}.
#'
#' @references
#' Van Dyke, C. W., White, R. D., Obuchowski, N. A., Geisinger, M. A., Lorig, R. J., & Meziane, M. A. (1993). Cine MRI in the diagnosis of thoracic aortic dissection. 79th RSNA Meetings. *Chicago, IL*, 28.
#'
#' @source This data are available at <http://perception.radiology.uiowa.edu> and <https://github.com/brian-j-smith/MRMCaov/tree/master/data>.
#'
#' @examples
#' ## Load example data
#' data(VanDyke)
#'
#' ## Return the first parts of an object
#' head(VanDyke)
#'
#' ## Extract unique modalities
#' unique(VanDyke$treatment)
#'
#' ## Extract Unique readers
#' unique(VanDyke$reader)
#'
#' ## Create binary test results (Y_ijk)
#' VanDyke$Y <- as.numeric(VanDyke$rating >= 3)
#'
#' @keywords datasets
"VanDyke"
