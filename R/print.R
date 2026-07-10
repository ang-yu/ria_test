#' @importFrom cli cli_div cli_rule cli_end cli_h3
#' @importFrom purrr iwalk
#' @export
print.ria.test <- function(x, ...) {
	cat("\n")
	d <- cli_div(theme = list(rule = list("line-type" = "double")))
	cli_rule(left = "Results {.fn ria.test}")
	cli_end(d)
	iwalk(x$estimates, print_estimate)
}

print_estimate <- function(x, name) {
	title <- switch(name,
									"ate" = "Average Treatment Effect",
									"direct" = "Direct Effect",
									"indirect" = "Indirect Effect",
									"p1" = "Path: A -> Y",
									"p2" = "Path: A -> Z -> Y",
									"p3" = "Path: A -> Z -> M -> Y",
									"p4" = "Path: A -> M -> Y",
									"intermediate_confounding" = "Intermediate Confounding",
									"ode" = "Organic Direct Effect",
									"oie" = "Organic Indirect Effect",
									"ride" = "Randomized Direct Effect",
									"riie" = "Randomized Indirect Effect",
									"rate" = "Randomized Average Effect",
									"ate_rate_diff" = "ATE-ATER")
	cli_h3("{.emph {title}}")

	est <- round(x@x, digits = 6)
	se <- round(x@std_error, digits = 6)
	ci <- round(x@conf_int, digits = 6)

	cat(sprintf("Estimate: %.6f\n", est))
	cat(sprintf("Std. Error: %.6f\n", se))
	cat(sprintf("95%% CI: [%.6f, %.6f]\n", ci[1], ci[2]))
}
