open Str
open Hashtbl

(** Reads a file line by line, and returns a list of lists of unique lines. *)
let file_to_list filename =
  let pickupLine = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      pickupLine := [ line ] :: !pickupLine
    done;
    !pickupLine
  with End_of_file ->
    close_in chan;
    List.rev !pickupLine

(** Randomizes the order of elements in a list. *)
let randomize_list lst =
  Random.self_init ();
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

(** Modify the `randomize_file` function to use the new `file_to_list` *)
let randomize_file filename =
  let unique_catchphrases_as_lists = file_to_list filename in
  randomize_list unique_catchphrases_as_lists

(** Tokenizes a sentence into words and punctuation, converting 
    everything to lowercase. *)
let tokenize_sentence sentence =
  let regexp = Str.regexp "\\(\\([?!.]\\)\\| \\|,\\)" in
  Str.full_split regexp sentence
  |> List.filter_map (function
       | Str.Text text -> Some (String.lowercase_ascii text)
       | Str.Delim delim when delim <> " " -> Some delim
       | _ -> None)

(** Tokenizes each sentence in a nested list of sentences. *)
let tokenize_nested_list nested_list =
  List.map
    (fun inner_list -> List.flatten (List.map tokenize_sentence inner_list))
    nested_list

(** Function to randomize a nested list. *)
let randomize_nested_list nested_lst =
  Random.self_init ();
  let shuffled = List.map Array.of_list nested_lst in
  let arr = Array.of_list shuffled in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr |> List.map Array.to_list

(** Counts frequencies of words in tokenized lines and returns a 
    hashtable of words frequencies *)
let count_word_frequencies tokenized_lines =
  let freqs = Hashtbl.create 10000 in
  List.iter
    (List.iter (fun token ->
         Hashtbl.replace freqs token
           (1 + try Hashtbl.find freqs token with Not_found -> 0)))
    tokenized_lines;
  freqs

(** Creates a vocabulary hashtable, adding tokens that exceed 
    a frequency threshold. *)
let adding_treshold tokenized_lines threshold =
  let freqs = count_word_frequencies tokenized_lines in
  let vocab = Hashtbl.create 10000 in
  let counter = ref 0 in
  Hashtbl.iter
    (fun token freq ->
      if freq > threshold then (
        Hashtbl.add vocab token !counter;
        incr counter)
      else if not (Hashtbl.mem vocab "<UNK>") then (
        Hashtbl.add vocab "<UNK>" !counter;
        incr counter))
    freqs;
  vocab

(** Constructs a vocabulary mapping each token to a unique integer. *)
let build_vocabulary tokenized_lines =
  let vocab = Hashtbl.create 10000 in
  let counter = ref 0 in
  let add_token token =
    if not (Hashtbl.mem vocab token) then (
      Hashtbl.add vocab token !counter;
      incr counter)
  in
  List.iter (List.iter add_token) tokenized_lines;
  vocab

(**  Converts tokenized lines into sequences of 
    integers based on the vocabulary. *)
let create_sequences tokenized_lines vocab =
  let convert_to_ints line vocab =
    List.map
      (fun token ->
        match Hashtbl.find_opt vocab token with
        | Some idx -> idx
        | None -> Hashtbl.find vocab "<UNK>")
      line
  in
  let input_seqs = ref [] in
  let target_seqs = ref [] in
  List.iter
    (fun line ->
      let int_line = convert_to_ints line vocab in
      let input_seq = List.rev (List.tl (List.rev int_line)) in
      let target_seq = List.tl int_line in
      input_seqs := input_seq :: !input_seqs;
      target_seqs := target_seq :: !target_seqs)
    tokenized_lines;
  (List.rev !input_seqs, List.rev !target_seqs)

(** One-hot encodes a token given the vocabulary size *)
let one_hot_encode token vocab_size =
  let vec = Array.make vocab_size 0.0 in
  vec.(token) <- 1.0;
  vec

(** Prepares data for RNN by converting sequences to one-hot encoded vectors *)
let prepare_data_for_rnn input_seqs target_seqs vocab_size =
  let encode_seq seq =
    List.map (fun token -> one_hot_encode token vocab_size) seq
  in
  let encoded_input_seqs = List.map encode_seq input_seqs in
  let encoded_target_seqs = List.map encode_seq target_seqs in
  (encoded_input_seqs, encoded_target_seqs)

(** Generates a padding token from a given vocabulary size. *)
let padding_token vocab_size = Array.make vocab_size 0.0

(** Takes the first n elements from a list *)
let take n lst =
  let rec aux n acc lst =
    match lst with
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | h :: t -> aux (n - 1) (h :: acc) t
  in
  aux n [] lst

(** Pads sequences to a length with a padding token. *)
let pad_sequences sequences max_length vocab_size =
  List.map
    (fun seq ->
      let padded =
        Array.to_list (Array.make max_length (padding_token vocab_size))
      in
      let len = min (List.length seq) max_length in
      List.rev
        (List.fold_left
           (fun acc x -> x :: acc)
           (List.tl (List.rev padded))
           (List.rev (take len seq))))
    sequences

(** Creates batches from a list with a specified batch size *)
let create_batches lst batch_size =
  let rec aux current_batch batches = function
    | [] when current_batch = [] -> List.rev batches
    | [] -> List.rev (current_batch :: batches)
    | h :: t ->
        if List.length current_batch < batch_size then
          aux (h :: current_batch) batches t
        else aux [ h ] (current_batch :: batches) t
  in
  aux [] [] lst |> List.map List.rev

(** Creates batches with padded sequences. *)
let create_padding sequences batch_size max_length vocab_size =
  let padded_sequences = pad_sequences sequences max_length vocab_size in
  create_batches padded_sequences batch_size

(** Sorts a list of integers in ascending order. *)
let sort_list lst = List.sort compare lst

(** Calculates the specified percentile of a list of integers. *)
let percentile p lst =
  if p < 0. || p > 100. then
    invalid_arg "percentile: p must be between 0 and 100";
  let sorted = sort_list lst in
  let index = int_of_float (p /. 100. *. float_of_int (List.length sorted)) in
  List.nth sorted (min (max index 0) (List.length sorted - 1))

(** Calculates the total number of unique words/tokens.*)
let vocab_size vocab = Hashtbl.length vocab
