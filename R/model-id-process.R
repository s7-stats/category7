#' @importFrom statim model_processor
S7::method(model_processor, cont_tab) = function(x, data = NULL, ...) {
    cat1_df = if (!is.null(data) && is.data.frame(data)) {
        cols = tidyselect::eval_select(expr = x@cat1, data = data)
        data[, cols, drop = FALSE]
    } else {
        quo_resolver(x@cat1)
    }
    cat2_df = if (!is.null(data) && is.data.frame(data)) {
        cols = tidyselect::eval_select(expr = x@cat2, data = data)
        data[, cols, drop = FALSE]
    } else {
        quo_resolver(x@cat2)
    }

    n1 = ncol(cat1_df)
    n2 = ncol(cat2_df)
    nms1 = names(cat1_df)
    nms2 = names(cat2_df)

    if (x@strict && (n1 > 1L || n2 > 1L)) {
        cli::cli_abort(c(
            "{.code strict = TRUE} allows only one variable per side.",
            "x" = "{.code cat1} has {n1} variable{?s}, {.code cat2} has {n2}.",
            "i" = "Use {.code strict = FALSE} to allow multiple variables."
        ))
    }

    if (x@strict) {
        list(
            cat1_nm = nms1[[1L]],
            cat2_nm = nms2[[1L]],
            tab = table(cat1_df[[1L]], cat2_df[[1L]])
        )
    } else {
        idx1 = rep(seq_len(n1), times = n2)
        idx2 = rep(seq_len(n2), each = n1)

        tabs = vector("list", n1 * n2)
        nms = character(n1 * n2)

        for (k in seq_along(tabs)) {
            i = idx1[[k]]
            j = idx2[[k]]
            tabs[[k]] = table(cat1_df[[i]], cat2_df[[j]])
            nms[[k]] = paste0(nms1[[i]], "_x_", nms2[[j]])
        }

        list(
            cat1_nm = nms1,
            cat2_nm = nms2,
            tabs = rlang::set_names(tabs, nms)
        )
    }
}

quo_resolver = function(quo) {
    expr = rlang::quo_get_expr(quo)
    env = rlang::quo_get_env(quo)

    if (rlang::is_symbol(expr)) {
        nm = rlang::as_string(expr)
        vctrs::new_data_frame(
            list(rlang::eval_tidy(expr, env = env)) |>
                rlang::set_names(nm)
        )
    } else if (rlang::is_call(expr, "c")) {
        vars = as.list(expr[-1])
        nms = vapply(vars, rlang::as_string, character(1))
        lapply(vars, function(v) {
            rlang::eval_tidy(rlang::new_quosure(v, env = env), env = env)
        }) |>
            rlang::set_names(nms) |>
            vctrs::new_data_frame()
    } else {
        expr_lbl = rlang::as_label(quo)
        cli::cli_abort(c(
            "Invalid input in model ID: {.code {expr_lbl}}.",
            "i" = "Use bare names or {.code c()} for column references."
        ))
    }
}
