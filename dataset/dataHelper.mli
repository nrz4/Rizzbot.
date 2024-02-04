val file_to_list : string -> string list list
(** Reads a file line by line and returns a list of lines. *)

val randomize_list : 'a list -> 'a list
(** Randomizes the order of elements in a list. *)

val randomize_file : string -> string list list
(** Reads and randomizes the order of lines from a file. *)

val tokenize_sentence : string -> string list
(** Tokenizes a sentence into words and punctuation,
    converting everything to lowercase. *)

val tokenize_nested_list : string list list -> string list list
(** Tokenizes each sentence in a nested list of sentences. *)

val randomize_nested_list : 'a list list -> 'a list list
(** Function to randomize a nested list (list of lists). *)

val count_word_frequencies : string list list -> (string, int) Hashtbl.t
(** Counts frequencies of words in tokenized lines and 
    returns a hashtable of word frequencies. *)

val adding_treshold : string list list -> int -> (string, int) Hashtbl.t
(** Creates a vocabulary hashtable, adding tokens that 
    exceed a frequency threshold. *)

val build_vocabulary : string list list -> (string, int) Hashtbl.t
(** Constructs a vocabulary mapping each token to a unique integer. *)

val create_sequences :
  string list list -> (string, int) Hashtbl.t -> int list list * int list list
(** Converts tokenized lines into sequences of integers 
    based on the vocabulary. *)

val one_hot_encode : int -> int -> float array
(** One-hot encodes a token given the vocabulary size. *)

val prepare_data_for_rnn :
  int list list ->
  int list list ->
  int ->
  float array list list * float array list list
(** Prepares data for RNN by converting sequences to one-hot encoded format. *)

val padding_token : int -> float array
(** Generates a padding token (zero vector) for a given vocabulary size. *)

val pad_sequences : float array list list -> int -> int -> float array list list
(** Pads sequences to a specified length with a padding token. *)

val take : int -> 'a list -> 'a list
(** Takes the first n elements from a list. *)

val create_batches : 'a list -> int -> 'a list list
(** Creates batches from a list with a specified batch size. *)

val create_padding :
  float array list list -> int -> int -> int -> float array list list list
(** Creates batches with padded sequences. *)

val sort_list : int list -> int list
(** Sorts a list of integers in ascending order. *)

val percentile : float -> int list -> int
(** Calculates the specified percentile of a list of integers. *)

val vocab_size : ('a, 'b) Hashtbl.t -> int
(** Calculates the total number of unique words/tokens.*)
