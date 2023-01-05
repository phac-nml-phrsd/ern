test_that("distributions are initialized correctly", {
  for (f in ls(getNamespace("ern"), pattern = "def_dist_")) {
    expect_type(get(f)(), "list")
    expect_equal(names(get(f)())[1], "dist")
  }
})
