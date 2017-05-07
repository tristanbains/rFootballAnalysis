context("ST_Ranking")

data = DL_MatchData("ESP1",season1 = "2014-2015",season2 = "2014-2015")
result = ST_Ranking(data = data)

test_that("Right ordering based on aggregate matches",{
  expect_equal(result$GD[16:18],c(-25,-35,-21))
})
