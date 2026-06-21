chi2test_def = statim::stat_define(
    model_type = cont_tab,
    impl = statim::agendas(
        base = statim::baseline(
            fn = function(.proc, correct = TRUE) {
                tab = .proc$tab

                stats::chisq.test(tab, correct = correct)
            }
        )
    )
)
