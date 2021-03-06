options( # for compatibility with R < 4.0
  stringsAsFactors = FALSE
)
tol <- 1e-5
test_that("flatten_corr_mat works", {
  corr_mat <- data.frame(
    feature1 = c("a", "a", "b"),
    feature2 = c("b", "c", "c"),
    corr = c(0.8, 0.4, 0)
  )
  expect_equivalent(flatten_corr_mat(t(data.frame(
    a = c(1, 0.8, 0.4),
    b = c(0.8, 1, 0),
    c = c(0.4, 0, 1)
  ))),
  corr_mat,
  tolerance = tol
  )
})

test_that("get_corr_feats works", {
  cor_feats <- data.frame(
    feature1 = c("a"),
    feature2 = c("b"),
    corr = c(0.8)
  )
  expect_equivalent(get_corr_feats(data.frame(
    a = c(0.8966972, 0.2655087, 0.37212390, 0.5728534),
    b = c(0.9082078, 0.2016819, 0.89838968, 0.9446753),
    c = c(0.6607978, 0.6291140, 0.06178627, 0.2059746)
  ), 0.6), cor_feats, tolerance = tol)
  expect_equivalent(get_corr_feats(data.frame(
    a = c(1, 1, 0, 0),
    b = c(1, 1, 0, 0),
    c = c(0, 0, 1, 1)
  )), structure(list(feature1 = c("a", "a", "b"), feature2 = c(
    "b",
    "c", "c"
  ), corr = c(1, -1, -1)), class = "data.frame", row.names = c(
    NA,
    -3L
  )), tolerance = tol)
  expect_equivalent(get_corr_feats(data.frame(
    a = c(1, 1, 0, 0),
    b = c(1, 1, 0, 0),
    c = c(0, 0, 1, 1)
  ), group_neg_corr = FALSE), structure(list(feature1 = "a", feature2 = "b", corr = 1), class = "data.frame", row.names = c(
    NA,
    -1L
  )), tolerance = tol)
})

test_that("group_correlated_features works", {
  expect_equal(
    sort(group_correlated_features(data.frame(a = 1:3, b = 2:4, c = c(1, 0, 1)))),
    c("b|a", "c")
  )
  expect_equal(
    sort(group_correlated_features(data.frame(a = 1:3, b = c(3, 1, 2)))),
    c("a", "b")
  )

  corr <- dplyr::tibble(feature1 = c(character()), feature2 = character())
  feature <- dplyr::tibble()
  expect_equal(group_correlated_features(feature), list())
})
