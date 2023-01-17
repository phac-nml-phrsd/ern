test_that("get_fecal_shedding detects incorrect pathogen",
          {
            p = "sick"
            expect_error(get_fecal_shedding(pathogen = p),
                         "Pathogen not found. Aborting!")
          })
