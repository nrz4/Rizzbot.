open DataHelper
open Rnn
open Hashtbl

(*************************************************************************)

(** Define variables outside the runner code. *)
let complete_tokenized_data =
  tokenize_nested_list (randomize_file "dataset/data/validation.txt")

(** Gives us the line lengths*)
let line_lengths =
  List.flatten
    (List.map
       (fun lines ->
         List.map (fun line -> List.length (tokenize_sentence line)) lines)
       complete_tokenized_data)

let p95_length = percentile 95. line_lengths
let vocab_creation = adding_treshold complete_tokenized_data 2
let size = vocab_size vocab_creation

let input_seqs, target_seqs =
  create_sequences complete_tokenized_data vocab_creation

let encoded_input_seqs, encoded_target_seqs =
  prepare_data_for_rnn input_seqs target_seqs size

let batched_input_seqs = create_padding encoded_input_seqs 32 p95_length size
let batched_target_seqs = create_padding encoded_target_seqs 32 p95_length size
let my_rnn = init_rnn size 512 size
let epochs = 50
let learning_rate = 0.01
(***************************************************************************)

(** This helps randomize the data right before a new epoch comes into play.*)
let prepare_data_for_epoch vocab =
  let shuffled_data = randomize_nested_list complete_tokenized_data in
  let input_seqs, target_seqs = create_sequences shuffled_data vocab_creation in
  let encoded_input_seqs, encoded_target_seqs =
    prepare_data_for_rnn input_seqs target_seqs size
  in
  ( create_padding encoded_input_seqs 32 p95_length size,
    create_padding encoded_target_seqs 32 p95_length size )

(** Define train_rnn function *)
let train_rnn rnn epochs learning_rate batched_input_seqs batched_target_seqs =
  Printf.printf "Training has started...\n";
  flush stdout;
  for epoch = 1 to epochs do
    Printf.printf "Starting Epoch: %d\n" epoch;
    let batched_input_seqs, batched_target_seqs =
      prepare_data_for_epoch vocab_creation
    in
    let current_lr = learning_rate_decay learning_rate epoch epochs in

    let epoch_loss = ref 0.0 in
    let batch_count = ref 0 in

    List.iter2
      (fun batch_input batch_target ->
        let batch_loss = ref 0.0 in
        let batch_size = List.length batch_input in
        batch_count := !batch_count + 1;

        List.iter2
          (fun input_sequence target_sequence ->
            let predicted_outputs =
              List.map (fun input -> rnn_step rnn input 0.3) input_sequence
            in
            let predicted_array = Array.of_list predicted_outputs in
            let target_array = Array.of_list target_sequence in
            let loss =
              categorical_cross_entropy_loss predicted_array target_array
            in
            batch_loss := !batch_loss +. loss;
            bptt rnn
              (Array.of_list input_sequence)
              (Array.of_list target_sequence)
              current_lr)
          batch_input batch_target;

        epoch_loss := !epoch_loss +. !batch_loss)
      batched_input_seqs batched_target_seqs;

    let avg_epoch_loss = !epoch_loss /. float_of_int !batch_count in
    Printf.printf "Epoch: %d, Average Epoch Loss: %f\n" epoch avg_epoch_loss
  done;
  Printf.printf "Training completed.\n"

(** Find the index of the first element in an array
   that satisfies the given predicate/ previous word*)
let find_index predicate array =
  let rec aux idx =
    if idx >= Array.length array then -1
    else if predicate array.(idx) then idx
    else aux (idx + 1)
  in
  aux 0

(** Softmax function with temperature *)
let softmax_with_temp temp logits =
  let max_logit = Array.fold_left max logits.(0) logits in
  let exps = Array.map (fun x -> exp ((x -. max_logit) /. temp)) logits in
  let sum_exps = Array.fold_left ( +. ) 0.0 exps in
  Array.map (fun x -> x /. sum_exps) exps

(** Sample an index from a probability distribution *)
let sample_from_distribution distribution =
  let sum = Array.fold_left ( +. ) 0.0 distribution in
  let normalized = Array.map (fun x -> x /. sum) distribution in
  let cumulative = Array.make (Array.length distribution) 0.0 in
  let _ =
    Array.fold_left
      (fun acc prob ->
        let new_acc = acc +. prob in
        cumulative.(find_index (fun x -> x = prob) normalized) <- new_acc;
        new_acc)
      0.0 normalized
  in
  let rand = Random.float 1.0 in
  let rec find_index cumul idx =
    if idx >= Array.length cumul then Array.length cumul - 1
    else if cumul.(idx) >= rand then idx
    else find_index cumul (idx + 1)
  in
  find_index cumulative 0

(** Function to generate text from the trained RNN *)
let generate_text rnn seed_text max_length vocab vocab_inv temp =
  let seed_tokens = tokenize_sentence seed_text in
  let seed_indices =
    List.map
      (fun token ->
        match Hashtbl.find_opt vocab token with
        | Some idx -> idx
        | None -> Hashtbl.find vocab "<UNK>")
      seed_tokens
  in
  let seed_vector =
    List.map (fun idx -> one_hot_encode idx size) seed_indices
  in
  let generated_text = ref seed_text in

  let current_input = ref (List.hd (List.rev seed_vector)) in

  for _ = 1 to max_length do
    let logits = rnn_step rnn !current_input 0.2 in
    let output_probs = softmax_with_temp temp logits in
    let next_index = sample_from_distribution output_probs in
    let next_token =
      match Hashtbl.find_opt vocab_inv next_index with
      | Some token -> if token = "<UNK>" then "cutie" else token
      | None -> "cutie"
    in
    generated_text :=
      !generated_text
      ^
      if String.contains ",.?!;:" next_token.[0] then next_token
      else " " ^ next_token;

    current_input := one_hot_encode next_index size
  done;

  !generated_text

(** Interactive Text where User gives in input and they give output *)
let interactive_text_generation rnn vocab vocab_inv =
  Printf.printf "Enter seed text (type 'exit' to quit):\n";
  let rec loop () =
    let seed_text = read_line () in
    if seed_text <> "exit" then (
      let generated = generate_text rnn seed_text 15 vocab vocab_inv 1.0 in
      Printf.printf "Generated Text: %s\n" generated;
      loop ())
  in
  loop ()

let () =
  train_rnn my_rnn epochs learning_rate batched_input_seqs batched_target_seqs;
  let vocab_inv = Hashtbl.create size in
  Hashtbl.iter (fun key value -> Hashtbl.add vocab_inv value key) vocab_creation;

  interactive_text_generation my_rnn vocab_creation vocab_inv
