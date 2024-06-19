test_that("mle works", {
  jl_set.seed(13)
  m_sim = vam(Time & Type ~ (ARAInf(.4) | Weibull(.001,2.5)))
  df = rand(m_sim,10)
  m_mle = vam(Time & Type ~ (ARAInf(.4) | Weibull(.001,2.5)))
  mle(m_mle, data = df)
  expect_true(all.equal(c( 0.001, 3.7640879038885426, 0.3210698323272919), params(m_mle), tolerance=1e-7))
})
