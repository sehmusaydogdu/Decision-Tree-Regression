# read data into memory
data_set <- read.csv("hw05_data_set.csv")

# get X and y values
X <- data_set$eruptions
y <- data_set$waiting

#split training and test data points
X_train <- as.matrix(X)[0:150,]
y_train <- as.matrix(y)[0:150,]
X_test  <- as.matrix(X)[151:272,]
y_test  <- as.matrix(y)[151:272,]

#pre-pruning parameter
pre_pruning_values <- list(5,10,15,20,25,30,35,40,45,50)
rmse_result_list<-c()
rmse_index <- 0
P <- 25

for(p_value in pre_pruning_values){
  P <- p_value
  # create necessary data structures
  node_indices <- list()
  is_terminal <- c()
  need_split <- c()
  
  node_features <- c()
  node_splits <- c()
  node <- c()
  
  # put all training instances into the root node
  node_indices <- list(1:length(X_train))
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  
  # learning algorithm
  while (1) {
    # find nodes that need splitting
    split_nodes <- which(need_split)
    # check whether we reach all terminal nodes
    if (length(split_nodes) == 0) {
      break
    }
    # find best split positions for all nodes
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node[split_node] <- mean(y_train[data_indices])
      # check whether node is pure
      if (length(data_indices) <= P) {
        is_terminal[split_node] <- TRUE
      } else {
        is_terminal[split_node] <- FALSE
          
        score <- 0
        split <- 0
          
        unique_values <- sort(unique(X_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
         
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
          split_scores[s] <- sum((y_train[left_indices] - mean(y_train[left_indices]))**2 / length(y_train[data_indices])) +
            sum((y_train[right_indices] - mean(y_train[right_indices]))**2 / length(y_train[data_indices]))
          -length(right_indices) / length(data_indices) * sum(sapply(1:max(y), function(c) {mean(y_train[right_indices] == c) * log2(mean(y_train[right_indices] == c))}), na.rm = TRUE)
        }
        score <- min(split_scores)
        split <- split_positions[which.min(split_scores)]
          
        node_splits[split_node] <- split
          
        # create left node using the selected split
        left_indices <- data_indices[which(X_train[data_indices] < split)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
          
        # create right node using the selected split
        right_indices <- data_indices[which(X_train[data_indices] >= split)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  
  # traverse tree for test data points
  y_predicted <- rep(0, length(X_test))
  for (i in 1:length(X_test)) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE) {
        y_predicted[i] <- node[index]
        break
      } else {
        if (X_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  
  #Plot data point
  plot(X_train, y_train, pch = 19,
       xlim = c(min(X_train), max(X_train)),
       ylim = c(min(y_train), max(y_train)),
       xlab = sprintf("Eruption time (min)", P), 
       ylab = "Waiting time to next encruption (min)", 
       main = sprintf("P = %g", P),
       col = "blue")
  points(X_test,y_test, pch = 19,col = "red")
  data_interval <- seq(from = min(X)-0.1, to = max(X)+0.1, by = 0.01)
  p_head <- rep(0, length(data_interval))
  
  for (i in 1:length(data_interval)) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE) {
        p_head[i] <- node[index]
        break
      } else {
        if (data_interval[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
  rmse <- sqrt(sum((y_test - y_predicted)**2) / length(y_test))
  print(sprintf("RMSE is %g when P is %g", rmse ,P))
  
  temp<-rmse
  rmse_result_list <-c(rmse_result_list,temp)
  rmse_index <- rmse_index+1
}

#Plot pre-pruning and RMSE representation
plot(pre_pruning_values,rmse_result_list, pch = 19,
     xlim = c(5, 50),
     ylim = c(min(rmse_result_list), max(rmse_result_list)),
     xlab = sprintf("Pre-pruning size (P)"), 
     ylab = "RMSE", 
     col = "black")
lines(pre_pruning_values,rmse_result_list,lwd=2, col="black")

