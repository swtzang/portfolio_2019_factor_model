#==================================================================================================================
# https://towardsdatascience.com/custom-factor-models-build-your-own-in-r-with-a-few-lines-of-codes-502274ae3624
#
#################################################################
#  LOAD THE DATA
#################################################################
# EXPOSURE DATA
data_filename = "data.csv"
factor_data <- data.frame(read.csv(data_filename))
# MATRIX DATA
constraints_matrix_filename = "constraints_matrix.csv" 
constraints_matrix <- data.frame(read.csv(constraints_matrix_filename))
constraints_matrix <-  as.matrix(constraints_matrix[ ,!(colnames(constraints_matrix) %in% c("X"))])