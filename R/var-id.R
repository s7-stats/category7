#' Contingency table of two variables
#'
#' `cont_tab()` creates a `cont_tab` model ID that reads as "the contingency table of
#' `cat1` and `cat2`".
#'
#' @param cat1 The first categorical variable of the contingency table.
#' @param cat2 The second categorical variable of the contingency table.
#' @param strict If `TRUE` (default), only a single variable is allowed for both
#'   `cat1` and `cat2`. Set to `FALSE` to allow multiple variables, producing a
#'   contingency table for each pair.
#'
#' @return An `cont_tab` / `var_id` S7 object.
#'
#' @examples
#' # Bare names
#' set.seed(123)
#' sex = sample(rep(c("Male", "Female"), each = 50))
#' species = sample(c("dog", "cat", "bird"), length(sex), replace = TRUE)
#' ct = cont_tab(sex, species)
#' ct
#' statim::define_model(ct)
#'
#' @export
cont_tab = S7::new_class(
    "cont_tab",
    parent = statim::var_id,
    properties = list(
        cat1 = S7::class_any,
        cat2 = S7::class_any,
        strict = S7::new_property(S7::class_logical, default = TRUE)
    ),
    constructor = function(cat1, cat2, strict = TRUE) {
        S7::new_object(
            S7::S7_object(),
            cat1 = rlang::enquo(cat1),
            cat2 = rlang::enquo(cat2),
            strict = strict
        )
    }
)
