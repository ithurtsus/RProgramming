quiz1 <- function () {
  lapply(split(iris, iris$Species), function (x) {
    mean(x$Sepal.Length)
  })
}