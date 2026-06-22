#' Chi-Square Test
#'
#' `CHI2_TEST()` performs a chi-square test of independence for a contingency
#' table. If `CHI2_TEST` is supplied within the lazy-loaded piped/grammar syntax, supply
#' `CHI2_TEST` as a function within i.e. `prepare_test(.test = CHI2_TEST)` call.
#'
#' @param .var_id A variable mapper `<var_id>`. When supplied, the test executes immediately.
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation. See the
#'   **Arguments by variable mapper** section for the full list per path.
#'
#' @return A `cld_exec` object (in [statim::conclude()]), a `stat_infer_spec` object, or a
#'   `test_spec` when `.var_id = NULL`. By default, returns a [class_chi2_tab] object.
#'
#' @section Supported variable mapper `<var_id>`s:
#' - `cont_tab()`: chi-square test of independence between two categorical variables.
#'
#' @examples
#' sex = sample(rep(c("Male", "Female"), each = 50))
#' species = sample(c("dog", "cat", "bird"), length(sex), replace = TRUE)
#'
#' # eager
#' CHI2_TEST(cont_tab(sex, species))
#'
#' # piped/grammar syntax
#' define_model(cont_tab(sex, species)) |>
#'     prepare_test(CHI2_TEST) |>
#'     conclude()
#'
#' @seealso [class_chi2_tab] for result class slots. [conclude()], [auto_tidy()].
#'
#' @export
CHI2_TEST = statim::HTEST_FN(
    cls = "chi2_test",
    defs = list(chi2test_def),
    .name = "Chi-Square Test"
)

#' Structured result container for chi-square tests
#'
#' @description
#' An S7 class produced by [CHI2_TEST] piped/grammar syntaxs using [cont_tab()] as the
#' variable mapper `<var_id>`. Not constructed manually — use the piped/grammar syntax instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [CHI2_TEST]):
#'
#' - `var1`: name of the row variable.
#' - `var2`: name of the column variable.
#' - `tab`: observed contingency table (a `table` or `matrix`).
#' - `chi2_stat`: chi-square statistic.
#' - `df`: degrees of freedom.
#' - `p_val`: p-value.
#'
#' @seealso [CHI2_TEST], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_chi2_tab = S7::new_class(
    "chi2",
    parent = statim::class_stat_infer,
    properties = list(
        var1 = S7::class_character,
        var2 = S7::class_character,
        tab = S7::new_property(
            class = S7::class_any,
            validator = function(value) {
                if (!is.table(value) && !is.matrix(value))
                    "tab must be a table or matrix"
            }
        ),
        chi2_stat = S7::class_numeric,
        df = S7::class_numeric,
        p_val = S7::class_numeric
    )
)

S7::method(print, class_chi2_tab) = function(x, percentage = "all", expected = TRUE, ...) {
    pval_styler = function(x) {
        x_num = suppressWarnings(as.numeric(x$value))
        if (is.na(x_num) || x_num > 0.05) {
            cli::style_italic(x$value)
        } else if (x_num > 0.01) {
            cli::col_red(x$value)
        } else {
            cli::style_bold("<0.001")
        }
    }

    names(dimnames(x@tab)) = c(x@var1, x@var2)

    tabstats::cross_table(x@tab, percentage = percentage, expected = expected)
    cat("\n")

    cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
    tabstats::table_default(
        tibble::tibble(
            chi2_stat = round(x@chi2_stat, 4L),
            df = x@df,
            p_val = round(x@p_val, 4L)
        ),
        style_columns = tabstats::td_style(p_val = pval_styler)
    )
    cat("\n\n")

    invisible(x)
}

#' @export
S7::method(auto_tidy, class_chi2_tab) = function(x, ...) {
    tibble::tibble(
        statistic = x@chi2_stat,
        df = x@df,
        p_value = x@p_val
    )
}
