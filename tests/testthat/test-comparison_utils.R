# Tests for comparison utility functions: make_cmp_string

set_verbosity(0)

test_that("make_cmp_string function works correctly", {
  expect_error(make_cmp_string(sample_names = letters,
                               group_c = sample(c(TRUE,FALSE), 26, replace = TRUE),
                               group_t = sample(c(TRUE,FALSE), 24, replace = TRUE)),
               regexp = "group_t is different length than sample_names")
  expect_error(make_cmp_string(sample_names = letters,
                               group_c = sample(c(TRUE,FALSE), 24, replace = TRUE),
                               group_t = sample(c(TRUE,FALSE), 26, replace = TRUE)),
               regexp = "group_c is different length than sample_names")
  expect_error(make_cmp_string(letters, rep(c("TRUE","FALSE"), 13), rep(c(FALSE,TRUE), 13)), regexp = "group_c is not a boolean")
  expect_error(make_cmp_string(letters, rep(c(TRUE,FALSE), 13), rep(c("FALSE","TRUE"), 13)), regexp = "group_t is not a boolean")
  expect_equal(make_cmp_string(letters, rep(c(TRUE,FALSE), 13), rep(c(FALSE,TRUE), 13)), "a,c,e,g,i,k,m,o,q,s,u,w,y b,d,f,h,j,l,n,p,r,t,v,x,z")
})
