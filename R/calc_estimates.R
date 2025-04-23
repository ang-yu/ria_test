calc_estimates_natural <- function(eif_ns, weights) {
	list(
		direct = eif_ns[["100"]] - eif_ns[["000"]],		  # A -> Y
		indirect = eif_ns[["111"]] - eif_ns[["100"]],		# A -> M -> Y
		ate = eif_ns[["111"]] - eif_ns[["000"]]
	)
}

calc_estimates_organic <- function(eif_ns, weights) {
	list(
		ode = eif_ns[["101"]] - eif_ns[["000"]],
		oie = eif_ns[["111"]] - eif_ns[["101"]]
	)
}

calc_estimates_ri <- function(eif_rs, weights) {
	list(
		ride = eif_rs[["1100"]] - eif_rs[["0000"]],
		riie = eif_rs[["1111"]] - eif_rs[["1100"]]
	)
}

calc_estimates_rt <- function(eif_ns, eif_rs, weights) {
	ans <- list(
		p1 = eif_ns[["111"]] - eif_ns[["011"]], 		# A -> Y
		p2 = eif_rs[["0111"]] - eif_rs[["0011"]],   # A -> Z -> Y
		p3 = eif_rs[["0011"]] - eif_rs[["0010"]],		# A -> Z -> M -> Y
		p4 = eif_ns[["010"]] - eif_ns[["000"]],		  # A -> M -> Y
		intermediate_confounding =		              # Intermediate confounding
			eif_ns[["011"]] - eif_rs[["0111"]] +
			eif_rs[["0011"]] - eif_rs[["0011"]] +
			eif_rs[["0010"]] - eif_ns[["010"]],
		ate = eif_ns[["111"]] - eif_ns[["000"]]
	)

	ans$indirect <- ans$p3 + ans$p4
	ans$direct <- ans$p1 + ans$p2
	ans
}

calc_estimates_te <- function(eif_ns, eif_rs, weights) {
	list(
		ate = weighted.mean(eif_ns[, "111"] - eif_ns[, "000"], weights),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"],
		rate = weighted.mean(eif_rs[, "1111"] - eif_rs[, "0000"], weights),
		eif_rate = eif_rs[, "1111"] - eif_rs[, "0000"],
		ate_rate_diff = weighted.mean(eif_ns[, "111"] - eif_ns[, "000"] - (eif_rs[, "1111"] - eif_rs[, "0000"]), weights),
		eif_ate_rate_diff = eif_ns[, "111"] - eif_ns[, "000"] - (eif_rs[, "1111"] - eif_rs[, "0000"])
	)
}
