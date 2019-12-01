# read data into memory
data_set <- read.csv("hw05_data_set.csv", header=TRUE, sep=",")

#split train and test data_sets
train_data_set <- data_set[0:150,]
test_data_set <- data_set[151:272,]

# get eruption and waiting values
X_train <- train_data_set$eruptions
y_train <- train_data_set$waiting

X_test <- test_data_set$eruptions
y_test <- test_data_set$waiting

# get number of classes and number of features
# K <- max(y)
D <- ncol(X_train)


# get numbers of train and test samples
N_train <- length(X_train)
N_test <- length(X_test)

#pre-pruning parameter
P <- 25

# create necessary data structures
node_indices <- list()
is_terminal <- c()
need_split <- c()

node_features <- c()
node_splits <- c()
node_means <- c()

# put all training instances into the root node
node_indices <- list(1:N_train)
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
    node_means[split_node] <- mean(y_train[data_indices])
    # check whether node is pure
    if (length(unique(y_train[data_indices])) == 1 | length(data_indices) <= P) {
      is_terminal[split_node] <- TRUE
    } else {
      is_terminal[split_node] <- FALSE
      
      best_score <- 0
      best_split <- 0
      unique_values <- sort(unique(X_train[data_indices]))
      split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
      split_scores <- rep(0, length(split_positions))
      for (s in 1:length(split_positions)) {
        left_indices <- data_indices[which(X_train[data_indices] < split_positions[s])]
        right_indices <- data_indices[which(X_train[data_indices] >= split_positions[s])]
        split_scores[s] <- sum((y_train[left_indices] - mean(y_train[left_indices]))**2 / length(y_train[data_indices])) +
          sum((y_train[right_indices] - mean(y_train[right_indices]))**2 / length(y_train[data_indices]))
        
        # split_scores[s] <- -length(left_indices) / length(data_indices) * sum(sapply(1:K, function(c) {mean(y_train[left_indices] == c) * log2(mean(y_train[left_indices] == c))}), na.rm = TRUE) +
        #                    -length(right_indices) / length(data_indices) * sum(sapply(1:K, function(c) {mean(y_train[right_indices] == c) * log2(mean(y_train[right_indices] == c))}), na.rm = TRUE)
      }
      best_score <- min(split_scores)
      best_split <- split_positions[which.min(split_scores)]
      
      # decide where to split on which feature
      #split_d <- which(best_score)
      # node_features[split_node] <- split_d
      node_splits[split_node] <- best_split
      
      # create left node using the selected split
      left_indices <- data_indices[which(X_train[data_indices] < best_split)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      # create right node using the selected split
      right_indices <- data_indices[which(X_train[data_indices] >= best_split)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}


# traverse tree for test data points
y_predicted <- rep(0, N_test)
for (i in 1:N_test) {
  index <- 1
  while (1) {
    if (is_terminal[index] == TRUE) {
      y_predicted[i] <- node_means[index]
      break
    } else {
      if (X_test[i] < node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}
rmse <- round(sqrt(sum((y_test - y_predicted)**2) / length(y_test)),4)
print(sprintf("RMSE is %g when P is %g", rmse ,P))




