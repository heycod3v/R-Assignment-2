# 1. Construct and store a 4x2 matrix filled row-wise
matrix_data <- matrix(c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5), nrow = 4, ncol = 2, byrow = TRUE)

# 2. Confirm dimensions after removing one row
matrix_removed_row <- matrix_data[-1, ]
dim(matrix_removed_row)  # 3x2

# 3. Overwrite the second column with sorted values
matrix_data[, 2] <- sort(matrix_data[, 2])

# 4. Delete the fourth row and first column as a single-column matrix
result_4 <- as.matrix(matrix_data[-4, -1])

# 5. Store bottom four elements as a 2x2 matrix
new_matrix <- matrix(matrix_data[3:4, ], nrow = 2, ncol = 2)

# 6. Overwrite specific elements with -1/2 diagonal values
diag_values <- -0.5 * diag(new_matrix)
matrix_data[c(4, 1), 2] <- diag_values[2]
matrix_data[c(4, 1), 1] <- diag_values[1]

# 7. Compute valid matrix operations
A <- matrix(c(1, 2, 7), nrow = 1, ncol = 3)
B <- matrix(c(3, 4, 8), nrow = 1, ncol = 3)
t(A) %*% B
t(B) %% (A %% t(A))
(A %% t(A)) %% t(B)

# Q8. Store these two matrices:
matA <- matrix(data = c(1,2,7))
matB <- matrix(data = c(3,4,8))

# Which of the following multiplications are possible? For those that are, compute the result.

# Solution : 
# i. A.B
print(matA %*% matB) # Error in matA %*% matB : non-conformable arguments

# ii. t(A).B 
print(t(matA) %*% matB)

# iii. t(B).(A.t(A))
print(t(matB) %*% (matA %*% t(matA)))

# iv. (A.t(A)) . t(B)
print((matA %*% t(matA)) %*% t(matB)) # Error in (matA %*% t(matA)) %*% t(matB) : non-conformable arguments

# v. solve([(B.t(B)) + (A.t(A)) - 100 * diag(3)])
print(solve((matB %*% t(matB)) + (matA %*% t(matA)) - 100 * diag(3)))

# 9. Confirm A^-1 - A - I4 provides a zero matrix
A <- diag(c(2, 3, 5, -1))
I4 <- diag(4)
result_9 <- solve(A) - A - I4

# 10. Create a 3D array with six layers
array_3d <- array(seq(4.8, 0.1, length.out = 4 * 2 * 6), dim = c(4, 2, 6))

# 11. Extract fourth and first-row elements of the second column
extracted_elements <- array_3d[c(4, 1), 2, ]

# 12. Create a 2x2x2x3 array with a fourfold repetition of the second row
new_array <- array(rep(array_3d[2, , ], times = 4), dim = c(2, 2, 2, 3))

# 13. Delete the sixth layer from the array
modified_array <- array_3d[, , -6]

# 14. Overwrite elements with -99 in specific layers
modified_array[c(2, 4), 2, c(1, 3, 5)] <- -99

# 15. Construct a 5x3 matrix filled column-wise
matrix_15 <- matrix(1:15, nrow = 5, ncol = 3)

# 16. Add a new row and confirm dimensions
new_matrix_15 <- rbind(matrix_15, c(16, 17, 18))

# 17. Sort the first column in descending order
sorted_matrix <- matrix_15
sorted_matrix[, 1] <- sort(matrix_15[, 1], decreasing = TRUE)

# 18. Remove the second row and third column while ensuring a matrix
result_18 <- sorted_matrix[-2, -3, drop = FALSE]

# 19. Create a 2x2 matrix using the top elements of the last column
new_2x2 <- matrix(sorted_matrix[1:2, 3], nrow = 2)

# 20. Replace specified elements with the average of corner elements
corner_avg <- mean(c(matrix_15[1, 1], matrix_15[1, 3], matrix_15[5, 1], matrix_15[5, 3]))
matrix_15[c(2, 5), c(1, 3)] <- corner_avg

# 21. Compute the given matrix operation
M1 <- matrix(c(2, 5, 3, 7), nrow = 2)
M2 <- matrix(c(5, 10, 15, 20), nrow = 2)
result_21 <- 3 * (M1 - M2)

# 22. Compute valid multiplications
C <- matrix(c(1, 2, 3, 4), nrow = 2)
D <- matrix(c(5, 6), nrow = 2)
result_22_ii <- t(C) %*% D
result_22_iii <- t(D) %% (C %% t(C))

# 23. Confirm B^-1 - B - I3 gives a zero matrix
B <- diag(c(1, 2, -2))
I3 <- diag(3)
result_23 <- solve(B) - B - I3

# 24. Construct a 3D array and extract the elements of the first row in the third column
set.seed(123)
array_3d_random <- array(sample(1:9, 3 * 3 * 4, replace = TRUE), dim = c(3, 3, 4))
extracted_vector <- array_3d_random[1, 3, ]