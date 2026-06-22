#' Chi-square test
#'
#' @export
CHI2_TEST = statim::HTEST_FN(
    cls = "chi2_test",
    defs = list(chi2test_def),
    .name = "Chi-Square Test"
)

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

#' @export
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

    # ct = x@tab
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
