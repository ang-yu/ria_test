test_that("the package exposes only the paper's estimand sets", {
	params <- ria.test:::estimand_parameters

	expect_named(params, c("natural", "ria", "test"))
	expect_length(params$natural$natural, 3L)
	expect_length(params$natural$randomized, 0L)
	expect_length(params$ria$natural, 0L)
	expect_length(params$ria$randomized, 3L)
	expect_length(params$test$natural, 2L)
	expect_length(params$test$randomized, 3L)
})

test_that("natural effects use the natural decomposition", {
	eif <- list("111" = 8, "100" = 3, "000" = 1)
	estimates <- ria.test:::calculate_natural_effects(eif)

	expect_named(estimates, c("TE", "NIE", "NDE"))
	expect_equal(unlist(estimates), c(TE = 7, NIE = 5, NDE = 2))
})

test_that("RIA effects use the randomized interventional decomposition", {
	eif <- list("1111" = 9, "1100" = 4, "0000" = 2)
	estimates <- ria.test:::calculate_ria_effects(eif)

	expect_named(estimates, c("TE^R", "NIE^R", "NDE^R"))
	expect_equal(unlist(estimates), c("TE^R" = 7, "NIE^R" = 5, "NDE^R" = 2))
})

test_that("the falsification contrast is TE - TE^R", {
	natural_eif <- list("111" = 8, "000" = 1)
	ria_eif <- list("1111" = 9, "0000" = 3)
	estimates <- ria.test:::calculate_test_effects(natural_eif, ria_eif)

	expect_named(estimates, c("TE", "TE^R", "TE - TE^R"))
	expect_equal(unlist(estimates), c("TE" = 7, "TE^R" = 6, "TE - TE^R" = 1))
})

test_that("estimand requirements reflect identification", {
	check <- ria.test:::check_estimand_compatibility

	expect_true(check(NULL, "natural"))
	expect_true(check("l", "ria"))
	expect_true(check("l", "test"))
	expect_match(check(NULL, "test"), "Must provide")
	expect_match(check("l", "natural"), "not identified")
})
