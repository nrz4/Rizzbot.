type vector = float array
(** Basic types for vectors and matrices in RNN computations. *)

type matrix = vector array

val create_vector : int -> vector
(** Creates a vector of a specified size, initialized to zero. *)

val create_matrix : int -> int -> matrix
(** Creates a matrix with specified dimensions, initialized to zero. *)

val random_matrix : int -> int -> matrix
(** Generates a random matrix of specified dimensions with small 
    initial values, used for weight initialization. *)

type rnn_params = {
  mutable wih : matrix;
  mutable whh : matrix;
  mutable who : matrix;
  mutable bih : vector;
  mutable bho : vector;
  mutable h : vector;
}
(** Represents the structure of RNN parameters 
    including weights, biases, and hidden states. *)

val init_rnn : int -> int -> int -> rnn_params
(** Initializes RNN parameters with random weights and zero biases. *)

val sigmoid : float -> float
(** Sigmoid activation function. *)

val tanh : float -> float
(** Hyperbolic tangent activation function. *)

val dot_product : vector -> vector -> float
(** Computes the dot product of two vectors. *)

val matrix_vector_mul : matrix -> vector -> vector
(** Performs matrix-vector multiplication. *)

val dropout : float -> vector -> vector
(** Dropout function to randomly set a fraction of input units
     to 0 at each update during training time. *)

val vector_add : vector -> vector -> vector
(** Adds two vectors element-wise. *)

val rnn_step : rnn_params -> vector -> float -> vector
(** Performs one step of RNN forward pass, including dropout. *)

val categorical_cross_entropy_loss : vector array -> vector array -> float
(** Categorical cross-entropy loss function for one-hot encoded outputs. *)

val derivative_categorical_cross_entropy_loss :
  vector array -> vector array -> vector array
(** Derivative of the categorical cross-entropy loss function. *)

val derivative_tanh : float -> float
(** Derivative of the tanh function. *)

val update_matrix : matrix -> matrix -> float -> unit
(** Updates matrix parameters with gradients. *)

val update_vector : vector -> vector -> float -> unit
(** Updates vector parameters with gradients. *)

val bptt : rnn_params -> vector array -> vector array -> float -> unit
(** Backpropagation Through Time (BPTT) implementation for training RNNs. *)

val learning_rate_decay : float -> int -> int -> float
(** Function to make sure that the learning rate decreases over time tp prevent
    overfitting.*)
