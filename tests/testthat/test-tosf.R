context("test-tosf")

test_that("multiplication works", {
  df <- data.frame(id = c("Point1", "Point2", "Point3"),
                   lat = c(42.032974, 44.032323, 47.123123),
                   long = c(-93.581543, -92.58345343, -96.2324543),
                   stringsAsFactors = FALSE)
  result <- tosf(df=df,id_cn = "id", Lon_cn= "long", Lat_cn= "lat")
  expect_is(result, "sf")
  expect_is(result, "data.frame")
})
