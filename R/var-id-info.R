#' @importFrom statim var_id_info
S7::method(var_id_info, cont_tab) = function(.var_id, processed = NULL, ...) {
    cat1_lbl = ct_quo_label(.var_id@cat1)
    cat2_lbl = ct_quo_label(.var_id@cat2)

    other_info = list(strict = .var_id@strict)
    vars = list()

    if (!is.null(processed) && length(processed)) {
        if (.var_id@strict) {
            other_info$n_pairs = 1L
            vars = list(
                list(name = processed$cat1_nm, preview = "<cat>"),
                list(name = processed$cat2_nm, preview = "<cat>")
            )
        } else {
            other_info$n_pairs = length(processed$tabs)
            vars = c(
                lapply(processed$cat1_nm, function(nm) list(name = nm, preview = "<cat>")),
                lapply(processed$cat2_nm, function(nm) list(name = nm, preview = "<cat>"))
            )
        }
    }

    statim::class_model_inform(
        var_id = .var_id,
        args = paste(cat1_lbl, "|", cat2_lbl),
        other_info = other_info,
        vars = vars,
        registered = TRUE
    )
}

ct_quo_label = function(quo) {
    expr = rlang::quo_get_expr(quo)
    if (rlang::is_symbol(expr)) {
        as.character(expr)
    } else if (rlang::is_call(expr, "c")) {
        paste(vapply(as.list(expr[-1]), as.character, character(1)), collapse = ", ")
    } else {
        rlang::as_label(quo)
    }
}
