#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a ria.test object
#'
#' @param x A `ria.test` object produced by a call to [ria.test::ria.test()].
#' @param ... Unused, included for generic consistency only.
#'
#' @return A tidy [tibble::tibble()] summarizing information about the model.
#'
#' @example inst/examples/examples.R
#'
#' @importFrom purrr list_rbind map
#'
#' @export
tidy.ria.test <- function(x, ...) {
	out <- list_rbind(map(x$estimates, ife::tidy), names_to = "estimand")
	class(out) <- c("tbl_df", "tbl", "data.frame")
	out
}
