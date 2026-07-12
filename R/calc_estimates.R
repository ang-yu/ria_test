calculate_estimates <- function(estimand, eif_natural, eif_ria) {
	switch(
		estimand,
		natural = calculate_natural_effects(eif_natural),
		ria = calculate_ria_effects(eif_ria),
		test = calculate_test_effects(eif_natural, eif_ria)
	)
}

calculate_natural_effects <- function(eif) {
	list(
		TE = eif[["111"]] - eif[["000"]],
		NIE = eif[["111"]] - eif[["100"]],
		NDE = eif[["100"]] - eif[["000"]]
	)
}

calculate_ria_effects <- function(eif) {
	list(
		"TE^R" = eif[["1111"]] - eif[["0000"]],
		"NIE^R" = eif[["1111"]] - eif[["1100"]],
		"NDE^R" = eif[["1100"]] - eif[["0000"]]
	)
}

calculate_test_effects <- function(eif_natural, eif_ria) {
	te <- eif_natural[["111"]] - eif_natural[["000"]]
	te_r <- eif_ria[["1111"]] - eif_ria[["0000"]]
	nie_r <- eif_ria[["1111"]] - eif_ria[["1100"]]
	nde_r <- eif_ria[["1100"]] - eif_ria[["0000"]]

	list(
		TE = te,
		"TE^R" = te_r,
		"TE - TE^R" = te - te_r,
		"NIE^R" = nie_r,
		"NDE^R" = nde_r
	)
}
