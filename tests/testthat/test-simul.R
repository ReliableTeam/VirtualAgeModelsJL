test_that("simul works", {
  jl_set.seed(13)
  m_sim = vam(Time & Type ~ (ARAInf(.4) | Weibull(.001,2.5)))
  df = rand(m_sim,10)
  expect_true(all.equal(unname(unlist(df[1,])),c(20.43478, -1.00000), tolerance=1e-7))
})