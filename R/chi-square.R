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
        tab = S7::new_property(
            class = S7::class_any,
            validator = function(value) {
                if (is.table(value) || is.matrix(value))
                    "Contingency table is a type of matrix (or a table in R)"
            }
        ),
        chi2_stat = S7::class_numeric,
        df = S7::class_numeric,
        p_val = S7::class_numeric
    )
)
