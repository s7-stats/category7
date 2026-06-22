chi2test_def = statim::stat_define(
    model_type = cont_tab,
    impl = statim::agendas(
        base = statim::baseline(
            fn = function(.proc, correct = TRUE) {
                raw = stats::chisq.test(.proc$tab, correct = correct)

                class_chi2_tab(
                    var1 = .proc$cat1_nm,
                    var2 = .proc$cat2_nm,
                    tab = raw$observed,
                    chi2_stat = unname(raw$statistic),
                    df = unname(raw$parameter),
                    p_val = raw$p.value
                )
            }
        )
    )
)
