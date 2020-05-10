test_that("ObsLulcRasterStack construction works with pie data", {
  obs <- ObsLulcRasterStack(x=pie,
                            pattern="lu",
                            categories=c(1,2,3),
                            labels=c("forest","built","other"),
                            t=c(0,6,14))
  expect_is(obs, "ObsLulcRasterStack")
  expect_identical(length(obs@t), length(obs@layers))
})

test_that("ObsLulcRasterStack construction works with stacked rasters", {
  expect_is(ObsLulcRasterStack(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999),
                               categories=c(1,2,3),
                               labels=c("forest","built","other"),
                               t=c(0,6,14)), 
            "ObsLulcRasterStack")
})

test_that("ObsLulcRasterStack construction works with one raster", {
  expect_is(ObsLulcRasterStack(x=pie$lu_pie_1985,
                               categories=c(1,2,3),
                               labels=c("forest","built","other"),
                               t=c(0)), 
            "ObsLulcRasterStack")
})

test_that("ObsLulcRasterStack construction works with list of rasters", {
  expect_is(ObsLulcRasterStack(x=list(lu_pie_1985 = pie$lu_pie_1985, 
                                      lu_pie_1991 = pie$lu_pie_1991, 
                                      lu_pie_1999 = pie$lu_pie_1999),
                               pattern = "lu",
                               categories=c(1,2,3),
                               labels=c("forest","built","other"),
                               t=c(0,6,14)), 
            "ObsLulcRasterStack")
})

test_that("ObsLulcRasterStack construction fails when lists are not named", {
  expect_error(ObsLulcRasterStack(x=list(pie$lu_pie_1985, 
                                         pie$lu_pie_1991, 
                                         pie$lu_pie_1999),
                                  pattern = "lu",
                                  categories=c(1,2,3),
                                  labels=c("forest","built","other"),
                                  t=c(0,6,14)), 
               "list elements must be named")
  
  expect_error(ObsLulcRasterStack(x=c(pie$lu_pie_1985, 
                                      pie$lu_pie_1991, 
                                      pie$lu_pie_1999),
                                  pattern = "lu",
                                  categories=c(1,2,3),
                                  labels=c("forest","built","other"),
                                  t=c(0,6,14)),
               "list elements must be named") 
})

test_that("ObsLulcRasterStack construction fails when categories are wrong", {
  # Missing a category
  expect_error(ObsLulcRasterStack(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999),
                                  categories=c(1,2),
                                  labels=c("forest","built","other"),
                                  t=c(0,6,14)),
               "unknown categories in maps") 
  # Wrong category
  expect_error(ObsLulcRasterStack(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999),
                                  categories=c(1,2,4),
                                  labels=c("forest","built","other"),
                                  t=c(0,6,14)), 
               "unknown categories in maps") 
})

test_that("ObsLulcRasterStack construction fails when timesteps are wrong", {
  # Too few
  expect_error(ObsLulcRasterStack(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999),
                                  categories=c(1,2,3),
                                  labels=c("forest","built","other"),
                                  t=c(0,6)), 
               "timesteps do not correspond with maps")
  # Too many
  expect_error(ObsLulcRasterStack(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999),
                                  categories=c(1,2,3),
                                  labels=c("forest","built","other"),
                                  t=c(0,6,14,20)), 
               "timesteps do not correspond with maps")
})

## Need test for the FILES ("maps not found")
## Issue identified: categories should match labels which should all match the raster values
