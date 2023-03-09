test_that("check_prm.R returns a warning message and a value of NULL
          when users passes their own config for R calculations", {
            prm.R = list(
              CI = 0.95,
              window = 10,
              config.EpiEstim = EpiEstim::make_config(seed = 15)
            )
            expect_warning(
              check_prm.R(prm.R)
            )
            expect_equal(
              check_prm.R(prm.R),
              NULL
            )
          }
          )

test_that("check_dist returns an error when invalid distributions are
          passed, and returns NULL when valid distribution is passed", {
            fec = def_dist_fecal_shedding()
            fec.missing.shape = purrr::discard_at(fec, "shape")
            fec.sd = purrr::list_modify(fec,
                                        sd = 2)
            expect_error(
              check_dist(fec.missing.shape)
            )
            expect_error(
              check_dist(fec.sd)
            )
            expect_equal(
              check_dist(fec),
              NULL
            )
            }
          )

test_that("check_for_deconv returns an error when number of observations <
          length of distribution vector, and returns NULL when obs >=
          length(dist)", {
            fec = get_discrete_dist(
              def_dist_fecal_shedding()
            )
            n.obs = 1:33
            n.obs.error = n.obs[-1]
            expect_error(
              check_for_deconv(
                obs = n.obs.error,
                dist = fec
                )
            )
            expect_equal(
              check_for_deconv(
                obs = n.obs,
                dist = fec
              ),
              NULL
            )
          })

test_that("check_data_clin returns an error when date and count columns
          are missing, and returns NULL when both columns are present in
          dateframe", {
            dat = data.frame(
              date = as.Date(character()),
              count = integer()
            )
            dat.rm.date = dplyr::select(dat, -date)
            dat.rm.count = dplyr::select(dat, -count)
            expect_error(
              check_data_clin(dat.rm.date)
            )
            expect_error(
              check_data_clin(dat.rm.count)
            )
            expect_equal(
              check_data_clin(dat),
              NULL
            )
          })
