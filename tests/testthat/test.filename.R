test_that("validity of the file name",{
    expect_equal(make_filename(2014),"accident_2014.csv.bz2")
})
