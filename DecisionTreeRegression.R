data_set <- read.csv("hw05_data_set.csv")

data_x <- data_set$eruptions
data_y <- data_set$waiting

x_train <- as.matrix(data_x)[0:150,]
y_train <- as.matrix(data_y)[0:150,]

x_test <- as.matrix(data_x)[151:272,]
y_test <- as.matrix(data_y)[151:272,]

bin_width <- 25

left_borders  <- seq(from = 0, to = 25, by = 0.01)
right_borders <- seq(from = 0, to = 25 , by = 0.01)

p_head <- sapply(1:length(left_borders), function(b) {
  insideTheBox <- left_borders[b] < x_train & x_train <= right_borders[b]
  sum(insideTheBox * y_train) / sum(insideTheBox)
})

plot(x_train, y_train, pch = 19,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     xlab = sprintf("Eruption time (min)", bin_width), 
     ylab = "Waiting time to next encruption (min)", 
     main = sprintf("P = %g", bin_width),
     col = "blue")

points(x_test,y_test, pch = 19,col = "red")

y_test_prediction <- sapply(1: length(x_test), function(i) {
  y <- ((x_test[i]+1) / bin_width)+1/2+5
  p_head[y] 
})

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

rmse_calculator <- sqrt(sum((y_test - y_test_prediction)*(y_test - y_test_prediction)) / length(y_test))
print(sprintf("Regressogram => RMSE is %g when P is %g", rmse_calculator,bin_width))
