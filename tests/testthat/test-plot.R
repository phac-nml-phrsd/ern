test_that("plot_diagnostic_ww returns an object that has a class that includes
          'ggplot' and 'patchwork'", {
            load("../testdata/ww_test_params.RData")
            r.obj = estimate_R_ww(
              ww.conc = ww.conc,
              dist.fec = dist.fec,
              dist.gi = dist.gi,
              prm.smooth = prm.smooth
            )
            g = plot_diagnostic_ww(r.obj, caption = "test")
            expect_equal(
              "patchwork" %in% class(g),
              TRUE
            )
            expect_equal(
              "ggplot" %in% class(g),
              TRUE
            )
})
