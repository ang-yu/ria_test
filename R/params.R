natural_parameters <- list(
	c(j = "data_1", k = "data_1", l = "data_1"),
	c(j = "data_1", k = "data_0", l = "data_0"),
	c(j = "data_0", k = "data_0", l = "data_0")
)

total_effect_parameters <- natural_parameters[c(1, 3)]

ria_parameters <- list(
	c(i = "data_1zp", j = "data_1", k = "data_0", l = "data_0"),
	c(i = "data_0zp", j = "data_0", k = "data_0", l = "data_0"),
	c(i = "data_1zp", j = "data_1", k = "data_1", l = "data_1")
)

estimand_parameters <- list(
	natural = list(natural = natural_parameters, randomized = list()),
	ria = list(natural = list(), randomized = ria_parameters),
	test = list(natural = total_effect_parameters, randomized = ria_parameters)
)
