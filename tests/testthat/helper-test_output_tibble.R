#' Helper function to test tibble output
#'
#' @param tbl tibble being tested
#' @param col_name vector of expected column names
#' @param col_class vector of expected column classes (must be of same length as `col_name`)
#'
test_output_tibble <- function(
    tbl,
    col_name,
    col_class
){
  # check class of entire output
  expect_s3_class(tbl, "tbl_df")

  # check names
  expect_equal(names(tbl), col_name)

  # check col types
  for (i in 1:length(col_name)){
    expect_equal(
      class(tbl[[col_name[i]]]),
      col_class[i]
    )
  }
}
