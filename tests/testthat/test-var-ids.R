test_that("quo_resolver handles bare name", {
    sex = c("Male", "Female", "Male")
    quo = rlang::quo(sex)
    out = quo_resolver(quo)

    expect_s3_class(out, "data.frame")
    expect_equal(ncol(out), 1L)
    expect_equal(names(out), "sex")
    expect_equal(out[[1L]], sex)
})

test_that("quo_resolver handles c() multi-variable", {
    x1 = c("a", "b")
    x2 = c("c", "d")
    quo = rlang::quo(c(x1, x2))
    out = quo_resolver(quo)

    expect_s3_class(out, "data.frame")
    expect_equal(ncol(out), 2L)
    expect_equal(names(out), c("x1", "x2"))
    expect_equal(out[["x1"]], x1)
    expect_equal(out[["x2"]], x2)
})

test_that("quo_resolver rejects invalid input", {
    quo = rlang::quo(x + y)
    expect_error(quo_resolver(quo), class = "rlang_error")
})

# strict = TRUE (default) -------------------------------------------------------

test_that("model_processor (strict) returns correct structure without data", {
    sex = sample(rep(c("Male", "Female"), each = 5))
    species = sample(c("dog", "cat", "bird"), 10L, replace = TRUE)

    out = model_processor(cont_tab(sex, species))

    expect_named(out, c("cat1_nm", "cat2_nm", "tab"))
    expect_equal(out$cat1_nm, "sex")
    expect_equal(out$cat2_nm, "species")
    expect_s3_class(out$tab, "table")
    expect_equal(dim(out$tab), c(2L, 3L))
})

test_that("model_processor (strict) works with data frame input", {
    df = data.frame(
        sex = sample(rep(c("Male", "Female"), each = 5)),
        species = sample(c("dog", "cat", "bird"), 10L, replace = TRUE)
    )

    out = model_processor(cont_tab(sex, species), data = df)

    expect_named(out, c("cat1_nm", "cat2_nm", "tab"))
    expect_equal(out$cat1_nm, "sex")
    expect_equal(out$cat2_nm, "species")
    expect_s3_class(out$tab, "table")
})

test_that("model_processor (strict) rejects multiple cat1 variables", {
    x1 = c("a", "b")
    x2 = c("c", "d")
    y = c("p", "q")

    expect_error(
        model_processor(cont_tab(c(x1, x2), y)),
        class = "rlang_error"
    )
})

test_that("model_processor (strict) rejects multiple cat2 variables", {
    x = c("a", "b")
    y1 = c("p", "q")
    y2 = c("r", "s")

    expect_error(
        model_processor(cont_tab(x, c(y1, y2))),
        class = "rlang_error"
    )
})

test_that("model_processor (strict) rejects multiple variables on both sides", {
    x1 = c("a", "b")
    x2 = c("c", "d")
    y1 = c("p", "q")
    y2 = c("r", "s")

    expect_error(
        model_processor(cont_tab(c(x1, x2), c(y1, y2))),
        class = "rlang_error"
    )
})

# strict = FALSE ----------------------------------------------------------------

test_that("model_processor (non-strict) returns correct structure", {
    x1 = c("a", "b", "a")
    x2 = c("c", "d", "c")
    y1 = c("p", "q", "p")
    y2 = c("r", "s", "r")

    out = model_processor(cont_tab(c(x1, x2), c(y1, y2), strict = FALSE))

    expect_named(out, c("cat1_nm", "cat2_nm", "tabs"))
    expect_equal(out$cat1_nm, c("x1", "x2"))
    expect_equal(out$cat2_nm, c("y1", "y2"))
    expect_type(out$tabs, "list")
    expect_length(out$tabs, 4L)
})

test_that("model_processor (non-strict) produces correct Cartesian pairs", {
    x1 = c("a", "b", "a")
    x2 = c("c", "d", "c")
    y1 = c("p", "q", "p")
    y2 = c("r", "s", "r")

    out = model_processor(cont_tab(c(x1, x2), c(y1, y2), strict = FALSE))

    expect_equal(names(out$tabs), c("x1_x_y1", "x2_x_y1", "x1_x_y2", "x2_x_y2"))
    expect_s3_class(out$tabs[["x1_x_y1"]], "table")
    expect_s3_class(out$tabs[["x2_x_y2"]], "table")
})

test_that("model_processor (non-strict) single-pair behaves same as strict", {
    sex = sample(rep(c("Male", "Female"), each = 5))
    species = sample(c("dog", "cat", "bird"), 10L, replace = TRUE)

    strict_out = model_processor(cont_tab(sex, species))
    non_strict_out = model_processor(cont_tab(sex, species, strict = FALSE))

    expect_equal(strict_out$tab, non_strict_out$tabs[[1L]])
})
