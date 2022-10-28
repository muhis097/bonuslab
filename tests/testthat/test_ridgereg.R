#' @importFrom MASS lm.ridge

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("lenreg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda= lmms))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda= lla))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda= 1)

  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})



test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)

  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]),2), c(1.86, 1.55, 1.11))
})


test_that("coef() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1.2)
  ridgereg_built_in <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1.2)

  expect_true(all(round(unname(ridgereg_mod$coef()),2) %in% round(unname(coef(ridgereg_built_in)),2)))
})


test_that("Crosschecking with lm.ridge()", {
  ridgereg_mod1 <- ridgereg$new(Petal.Length ~ Species, data=iris, lambda=1.2)
  ridgereg_built_in1 <- MASS::lm.ridge(Petal.Length ~ Species, data=iris, lambda=1.2)

  ridgereg_mod2 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=2)
  ridgereg_built_in2 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=2)

  ridgereg_mod3 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  ridgereg_built_in3 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)

  ridgereg_mod4 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1.5)
  ridgereg_built_in4 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1.5)

  expect_true(all(round(unname(ridgereg_mod1$coef()),2) %in% round(unname(coef(ridgereg_built_in1)),2)))

  expect_true(all(round(unname(ridgereg_mod2$coef()),2) %in% round(unname(coef(ridgereg_built_in2)),2)))

  expect_true(all(round(unname(ridgereg_mod3$coef()),2) %in% round(unname(coef(ridgereg_built_in3)),2)))

  expect_true(all(round(unname(ridgereg_mod4$coef()),2) %in% round(unname(coef(ridgereg_built_in4)),2)))
})
