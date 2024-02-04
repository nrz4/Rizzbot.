open DataHelper

(** Define the vector and matrix types *)
type vector = float array
type matrix = vector array

(** Creates a vector of size initialized to zero *)
let create_vector size = Array.make size 0.0

(** Create a matrix of rows and cols dimensions initialized to zero *)
let create_matrix rows cols = Array.make_matrix rows cols 0.0

(** Create a random matrix with small values to initialize weights *)
let random_matrix rows cols =
  Array.init rows (fun _ -> Array.init cols (fun _ -> Random.float 0.1 -. 0.05))

type rnn_params = {
  mutable wih : matrix;
  mutable whh : matrix;
  mutable who : matrix;
  mutable bih : vector;
  mutable bho : vector;
  mutable h : vector;
}
(** Creation of rnn_params, used to access and change the main components *)

(** Initialize the rnn model. *)
let init_rnn input_size hidden_size output_size =
  {
    wih = random_matrix hidden_size input_size;
    whh = random_matrix hidden_size hidden_size;
    who = random_matrix output_size hidden_size;
    bih = create_vector hidden_size;
    bho = create_vector output_size;
    h = create_vector hidden_size;
  }

(** Sigmoid atcivation fucntion *)
let sigmoid x = 1. /. (1. +. exp (-.x))

(** Hyperbolic tangent activation function *)
let tanh x = (2. /. (1. +. exp (-2. *. x))) -. 1.

(** Vector addition *)
let vector_add v1 v2 = Array.map2 ( +. ) v1 v2

(** Dot product of two vectors *)
let dot_product v1 v2 = Array.fold_left ( +. ) 0.0 (Array.map2 ( *. ) v1 v2)

(** Matrix vector multiplication. Allows matrices to be multiplied *)
let matrix_vector_mul m v = Array.map (fun row -> dot_product row v) m

(** Dropout rate ensures that the model does not overfit. *)
let dropout rate vec =
  Array.map (fun x -> if Random.float 1.0 < rate then 0.0 else x) vec

(** Perform one step of RNN forward pass while having some dropout *)
let rnn_step params input dropout_rate =
  let input_with_dropout = dropout dropout_rate input in
  let h_new =
    vector_add
      (matrix_vector_mul params.wih input_with_dropout)
      (matrix_vector_mul params.whh params.h)
  in
  let h_new = Array.map tanh h_new in
  let h_new_with_dropout = dropout dropout_rate h_new in
  let output =
    vector_add (matrix_vector_mul params.who h_new_with_dropout) params.bho
  in
  let output = Array.map sigmoid output in
  params.h <- h_new;
  output

(** Categorical cross-entropy loss function for one-hot encoded outputs.
    More common for uses with text generation*)
let categorical_cross_entropy_loss predicted actual =
  let n = Array.length predicted in
  let loss = ref 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to Array.length predicted.(i) - 1 do
      loss := !loss -. (actual.(i).(j) *. log (max predicted.(i).(j) 1e-15))
    done
  done;
  !loss /. float_of_int n

(** Derivative of the categorical cross-entropy loss function *)
let derivative_categorical_cross_entropy_loss predicted actual =
  let n = Array.length predicted in
  Array.init n (fun i ->
      Array.init
        (Array.length predicted.(i))
        (fun j -> predicted.(i).(j) -. actual.(i).(j)))

(** Derivative of tanh *)
let derivative_tanh x = 1.0 -. (tanh x ** 2.0)

(** Update matrix to have the new gradients *)
let update_matrix parameter gradient learning_rate =
  let rows = Array.length parameter in
  if rows > 0 then
    let cols = Array.length parameter.(0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        parameter.(i).(j) <-
          parameter.(i).(j) -. (learning_rate *. gradient.(i).(j))
      done
    done

(** Update vector to have the new gradients *)
let update_vector parameter gradient learning_rate =
  let len = Array.length parameter in
  for i = 0 to len - 1 do
    parameter.(i) <- parameter.(i) -. (learning_rate *. gradient.(i))
  done

(** Function to clip the values in a vector in place *)
let clip_vector_in_place vector threshold =
  for i = 0 to Array.length vector - 1 do
    vector.(i) <- max (-.threshold) (min threshold vector.(i))
  done

(** Function to clip the values in a matrix in place *)
let clip_matrix_in_place matrix threshold =
  Array.iter (fun row -> clip_vector_in_place row threshold) matrix

(** Backpropagation Through Time (BPTT) implementation *)
let bptt rnn input_sequence target_sequence learning_rate =
  (*Initializing everything first*)
  let num_timesteps = Array.length input_sequence in
  let hidden_size = Array.length rnn.h in
  let input_size = Array.length rnn.wih.(0) in
  let output_size = Array.length rnn.who in
  let truncate_backprop_steps = 10 in
  let dL_dwih = create_matrix hidden_size input_size
  and dL_dwhh = create_matrix hidden_size hidden_size
  and dL_dwho = create_matrix output_size hidden_size
  and dL_dbih = create_vector hidden_size
  and dL_dbho = create_vector output_size in
  let dropout_rate = 0.2 in

  (* FOrward pass*)
  let outputs = Array.make num_timesteps (create_vector output_size) in
  let hidden_states = Array.make num_timesteps (create_vector hidden_size) in
  for t = 0 to num_timesteps - 1 do
    outputs.(t) <- rnn_step rnn input_sequence.(t) dropout_rate;
    hidden_states.(t) <- Array.copy rnn.h
  done;

  (* Backward pass *)
  for t = num_timesteps - 1 downto 0 do
    let dy =
      derivative_categorical_cross_entropy_loss
        [| outputs.(t) |]
        [| target_sequence.(t) |]
    in
    for i = 0 to output_size - 1 do
      for j = 0 to hidden_size - 1 do
        dL_dwho.(i).(j) <-
          dL_dwho.(i).(j) +. (dy.(0).(i) *. hidden_states.(t).(j))
      done;
      dL_dbho.(i) <- dL_dbho.(i) +. dy.(0).(i)
    done;
    let dL_dh = ref (Array.make hidden_size 0.0) in
    for i = 0 to hidden_size - 1 do
      for j = 0 to output_size - 1 do
        !dL_dh.(i) <- !dL_dh.(i) +. (dy.(0).(j) *. rnn.who.(j).(i))
      done
    done;

    (* Propagate gradients *)
    for t_step = t downto max 0 (t - truncate_backprop_steps) do
      let input =
        if t_step = 0 then create_vector input_size
        else input_sequence.(t_step - 1)
      in
      for i = 0 to hidden_size - 1 do
        for j = 0 to input_size - 1 do
          dL_dwih.(i).(j) <- dL_dwih.(i).(j) +. (!dL_dh.(i) *. input.(j))
        done;
        for j = 0 to hidden_size - 1 do
          dL_dwhh.(i).(j) <-
            dL_dwhh.(i).(j) +. (!dL_dh.(i) *. hidden_states.(t_step).(j))
        done;
        dL_dbih.(i) <- dL_dbih.(i) +. !dL_dh.(i)
      done;

      (* Compute gradients for next t_step *)
      if t_step > 0 then (
        let dh_next = Array.make hidden_size 0.0 in
        for i = 0 to hidden_size - 1 do
          dh_next.(i) <- dot_product !dL_dh rnn.whh.(i)
        done;
        dL_dh := dh_next)
    done
  done;

  clip_matrix_in_place dL_dwih 5.0;
  clip_matrix_in_place dL_dwhh 5.0;
  clip_matrix_in_place dL_dwho 5.0;
  clip_vector_in_place dL_dbih 5.0;
  clip_vector_in_place dL_dbho 5.0;

  (* Update weights and biases using the calculated gradients *)
  update_matrix rnn.wih dL_dwih learning_rate;
  update_matrix rnn.whh dL_dwhh learning_rate;
  update_matrix rnn.who dL_dwho learning_rate;
  update_vector rnn.bih dL_dbih learning_rate;
  update_vector rnn.bho dL_dbho learning_rate

(** Function to make sure that the learning rate decreases over time tp prevent
    overfitting.*)
let learning_rate_decay initial_lr epoch max_epochs =
  initial_lr *. (1. -. (float_of_int epoch /. float_of_int max_epochs))
