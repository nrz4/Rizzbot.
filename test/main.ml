open OUnit2
open DataHelper
open Hashtbl
open Rnn

(* Test Plan:
    We will use a combination of automatic testing with OUnit and manual testing
    to validate different aspects of the system. OUnit will be employed for unit
    testing, for the RNN module and Datahelper module.

    Manual testing will cover integration tests, user interactions, and
    additional scenarios that are not covered by unit tests for our robot.ml
    GUI, making sure users are able to interact with the rizz chat bot.

    Black-box testing will focus on testing the functionality from the external
    perspective without considering the internal structure of the modules
    especially in the case of the RNN model functions.
    Glass-box testing will involve testing specific paths and conditions
    within the specifically for the dataHelper functions

    The combination of automatic testing with OUnit and manual testing ensures a
    major validation of the system. OUnit helps catch small bugs
    within the two modules, while manual testing addresses scenarios
    and user interactions with the GUI. This approach provides a robust
    strategy to show the correctness of the entire application.
*)

(** Helper function to help with testing and get rid of repetition. *)
let test (name : string) (input : 'a) (expected_output : 'a) : test =
  name >:: fun _ -> assert_equal input expected_output

(** Helper function to check if the hashtbl are equal in length*)
let hashtbl_equal ht1 ht2 =
  Hashtbl.length ht1 = Hashtbl.length ht2
  && Hashtbl.fold
       (fun key value acc -> acc && Hashtbl.find_opt ht2 key = Some value)
       ht1 true

(** Initial first few test cases.*)
let data_test =
  [
    test "A one line checker for file to list."
      (file_to_list "dataset/data/oneLine.txt")
      [ [ "Hi my name is marc" ] ];
    test "A checker for file to list"
      (file_to_list "dataset/data/TwoOrMore.txt")
      [ [ "Hey, My name is marc." ]; [ "I was wondering how you were." ] ];
    test "yess"
      (tokenize_nested_list [ [ "Hi my name is marc" ] ])
      [ [ "hi"; "my"; "name"; "is"; "marc" ] ];
    test "Normal test case for tokenize_nested_list"
      (tokenize_nested_list
         [ [ "Hey My name is marc" ]; [ "I was wondering how you were" ] ])
      [
        [ "hey"; "my"; "name"; "is"; "marc" ];
        [ "i"; "was"; "wondering"; "how"; "you"; "were" ];
      ];
    test "Punctuation only"
      (tokenize_nested_list [ [ ".,!?" ] ])
      [ [ "."; ","; "!"; "?" ] ];
  ]

(** Test cases for randomize_list function *)
let test_randomize_list =
  [
    test "randomize_list empty list" (randomize_list [] |> List.sort compare) [];
    test "randomize_list single element"
      (randomize_list [ 1 ] |> List.sort compare)
      [ 1 ];
    test "randomize_list repeated elements"
      (randomize_list [ 1; 1; 1 ] |> List.sort compare)
      [ 1; 1; 1 ];
    test "randomize_list large list"
      (let lst = List.init 100 (fun i -> i) in
       randomize_list lst |> List.sort compare)
      (List.init 100 (fun i -> i));
    test "randomize_list preserve elements"
      (let lst = [ 5; 3; 7; 1 ] in
       randomize_list lst |> List.sort compare)
      (List.sort compare [ 5; 3; 7; 1 ]);
    test "randomize_list different data types"
      (let lst = [ Obj.repr "a"; Obj.repr "a"; Obj.repr "a" ] in
       randomize_list lst |> List.map Obj.obj)
      [ "a"; "a"; "a" ];
  ]

(** Test cases for tokenize_sentence function *)
let test_tokenize_sentence =
  [
    test "tokenize_sentence simple case"
      (tokenize_sentence "This is a test")
      [ "this"; "is"; "a"; "test" ];
    test "tokenize_sentence with punctuation"
      (tokenize_sentence "Hello, world!")
      [ "hello"; ","; "world"; "!" ];
    test "tokenize_sentence multiple punctuation"
      (tokenize_sentence "Wait, stop! Are you sure?")
      [ "wait"; ","; "stop"; "!"; "are"; "you"; "sure"; "?" ];
    test "tokenize_sentence with apostrophes"
      (tokenize_sentence "It's a programmer's life")
      [ "it's"; "a"; "programmer's"; "life" ];
    test "tokenize_sentence with hyphenated words"
      (tokenize_sentence "This is a well-known issue")
      [ "this"; "is"; "a"; "well-known"; "issue" ];
    test "tokenize_sentence empty sentence" (tokenize_sentence "") [];
  ]

(** Test cases for randomize_list function *)
let test_randomize_list =
  [
    test "randomize_list empty list" (randomize_list [] |> List.sort compare) [];
    test "randomize_list single element"
      (randomize_list [ 1 ] |> List.sort compare)
      [ 1 ];
    test "randomize_list repeated elements"
      (randomize_list [ 1; 1; 1 ] |> List.sort compare)
      [ 1; 1; 1 ];
    test "randomize_list large list"
      (let lst = List.init 100 (fun i -> i) in
       randomize_list lst |> List.sort compare)
      (List.init 100 (fun i -> i));
    test "randomize_list preserve elements"
      (let lst = [ 5; 3; 7; 1 ] in
       randomize_list lst |> List.sort compare)
      (List.sort compare [ 5; 3; 7; 1 ]);
    test "randomize_list different data types"
      (let lst = [ Obj.repr "a"; Obj.repr "a"; Obj.repr "a" ] in
       randomize_list lst |> List.map Obj.obj)
      [ "a"; "a"; "a" ];
  ]

(** Test cases for tokenize_nested_list function *)
let test_tokenize_nested_list =
  [
    test "tokenize_nested_list simple case"
      (tokenize_nested_list [ [ "Hello, world!"; "Bye." ] ])
      [ [ "hello"; ","; "world"; "!"; "bye"; "." ] ];
    test "tokenize_nested_list mixed sentences"
      (tokenize_nested_list
         [
           [ "Good morning, everyone!"; "How's the day?" ];
           [ "Let's get started." ];
         ])
      [
        [ "good"; "morning"; ","; "everyone"; "!"; "how's"; "the"; "day"; "?" ];
        [ "let's"; "get"; "started"; "." ];
      ];
    test "tokenize_nested_list consecutive punctuation"
      (tokenize_nested_list [ [ "Is it true?!"; "Yes!!" ] ])
      [ [ "is"; "it"; "true"; "?"; "!"; "yes"; "!"; "!" ] ];
    test "tokenize_nested_list with empty strings"
      (tokenize_nested_list [ [ ""; "nothing to say..."; "" ] ])
      [ [ "nothing"; "to"; "say"; "."; "."; "." ] ];
    test "tokenize_nested_list complex sentences"
      (tokenize_nested_list
         [
           [ "In 2023, the world changed." ];
           [ "Why?"; "Because of A.I., of course!" ];
         ])
      [
        [ "in"; "2023"; ","; "the"; "world"; "changed"; "." ];
        [
          "why";
          "?";
          "because";
          "of";
          "a";
          ".";
          "i";
          ".";
          ",";
          "of";
          "course";
          "!";
        ];
      ];
  ]

(** Test cases for randomize_nested_list function *)
let test_randomize_nested_list =
  [
    test "randomize_nested_list length preservation"
      (let lst = [ [ "a"; "b" ]; [ "c"; "d" ] ] in
       let randomized = randomize_nested_list lst in
       List.length randomized = List.length lst)
      true;
    test "randomize_nested_list element presence"
      (let lst = [ [ "a"; "b" ]; [ "c"; "d" ] ] in
       let randomized = randomize_nested_list lst in
       List.for_all
         (fun sublist ->
           List.for_all
             (fun item -> List.exists (List.mem item) randomized)
             sublist)
         lst)
      true;
    test "randomize_nested_list different lengths"
      (let lst = [ [ "a"; "b"; "c" ]; [ "d" ] ] in
       let randomized = randomize_nested_list lst in
       List.length randomized = List.length lst)
      true;
    test "randomize_nested_list single element lists"
      (let lst = [ [ "a" ]; [ "b" ] ] in
       let randomized = randomize_nested_list lst in
       List.length randomized = List.length lst)
      true;
    test "randomize_nested_list empty inner lists"
      (let lst = [ [ "a"; "b" ]; [] ] in
       let randomized = randomize_nested_list lst in
       List.length randomized = List.length lst)
      true;
    test "randomize_nested_list all unique elements"
      (let lst = [ [ "a" ]; [ "b"; "c" ] ] in
       let randomized = randomize_nested_list lst in
       List.for_all
         (fun sublist ->
           List.for_all
             (fun item -> List.exists (List.mem item) randomized)
             sublist)
         lst)
      true;
    test "randomize_nested_list some repeated elements"
      (let lst = [ [ "a"; "b" ]; [ "b"; "c" ] ] in
       let randomized = randomize_nested_list lst in
       List.length (List.filter (fun l -> List.mem "b" l) randomized) = 2)
      true;
  ]

(** Test cases for count_word_frequencies function *)
let test_count_word_frequencies =
  [
    test "count_word_frequencies empty"
      (count_word_frequencies [ [] ] |> Hashtbl.length)
      0;
    test "count_word_frequencies single word"
      (let freqs = count_word_frequencies [ [ "hello" ] ] in
       Hashtbl.find_opt freqs "hello")
      (Some 1);
    test "count_word_frequencies multiple words"
      (let freqs = count_word_frequencies [ [ "hello"; "world"; "hello" ] ] in
       Hashtbl.find_opt freqs "hello" = Some 2
       && Hashtbl.find_opt freqs "world" = Some 1)
      true;
    test "count_word_frequencies multiple lists"
      (let freqs =
         count_word_frequencies [ [ "hello"; "world" ]; [ "hello"; "goodbye" ] ]
       in
       Hashtbl.find_opt freqs "hello" = Some 2
       && Hashtbl.find_opt freqs "world" = Some 1
       && Hashtbl.find_opt freqs "goodbye" = Some 1)
      true;
  ]

(** Test case for build vocabulary *)
let test_build_vocabulary =
  [
    ( "build_vocabulary empty" >:: fun _ ->
      assert_bool "not equal"
        (hashtbl_equal (build_vocabulary [ [] ]) (Hashtbl.create 0)) );
    ( "build_vocabulary single word" >:: fun _ ->
      let h = Hashtbl.create 1 in
      Hashtbl.add h "hello" 0;
      assert_bool "not equal"
        (hashtbl_equal (build_vocabulary [ [ "hello" ] ]) h) );
    ( "build_vocabulary with duplicates" >:: fun _ ->
      let expected =
        let h = Hashtbl.create 2 in
        Hashtbl.add h "hello" 0;
        Hashtbl.add h "world" 1;
        h
      in
      assert_bool "not equal"
        (hashtbl_equal
           (build_vocabulary [ [ "hello"; "world"; "hello" ] ])
           expected) );
    ( "build_vocabulary multiple unique lists" >:: fun _ ->
      let expected =
        let h = Hashtbl.create 2 in
        Hashtbl.add h "hello" 0;
        Hashtbl.add h "world" 1;
        h
      in
      assert_bool "not equal"
        (hashtbl_equal (build_vocabulary [ [ "hello" ]; [ "world" ] ]) expected)
    );
    ( "build_vocabulary mixed duplicates and uniques" >:: fun _ ->
      let expected =
        let h = Hashtbl.create 3 in
        Hashtbl.add h "hello" 0;
        Hashtbl.add h "world" 1;
        Hashtbl.add h "goodbye" 2;
        h
      in
      assert_bool "not equal"
        (hashtbl_equal
           (build_vocabulary [ [ "hello"; "world" ]; [ "hello"; "goodbye" ] ])
           expected) );
    ( "build_vocabulary complex scenario" >:: fun _ ->
      let h = Hashtbl.create 6 in
      Hashtbl.add h "apple" 0;
      Hashtbl.add h "banana" 1;
      Hashtbl.add h "cherry" 2;
      Hashtbl.add h "date" 3;
      Hashtbl.add h "fig" 4;
      Hashtbl.add h "grape" 5;
      assert_bool "not equal"
        (hashtbl_equal
           (build_vocabulary
              [
                [ "apple"; "banana"; "cherry" ];
                [ "date"; "apple"; "fig"; "grape" ];
              ])
           h) );
  ]

(** Test cases for one_hot_encode function *)
let test_one_hot_encode =
  [
    test "one_hot_encode basic" (one_hot_encode 1 3) [| 0.0; 1.0; 0.0 |];
    test "one_hot_encode first element in small array" (one_hot_encode 0 3)
      [| 1.0; 0.0; 0.0 |];
    test "one_hot_encode last element in small array" (one_hot_encode 2 3)
      [| 0.0; 0.0; 1.0 |];
    test "one_hot_encode middle element in larger array" (one_hot_encode 4 10)
      [| 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0 |];
    test "one_hot_encode single element array" (one_hot_encode 0 1) [| 1.0 |];
  ]

(** Test cases for padding_token function *)
let test_padding_token =
  [
    test "padding_token basic" (padding_token 3) [| 0.0; 0.0; 0.0 |];
    test "padding_token for vocab size 1" (padding_token 1) [| 0.0 |];
    test "padding_token for larger vocab size" (padding_token 5)
      [| 0.0; 0.0; 0.0; 0.0; 0.0 |];
    test "padding_token for vocab size 10" (padding_token 10)
      [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 |];
    test "padding_token for vocab size 0" (padding_token 0) [||];
    test "padding_token for arbitrary vocab size" (padding_token 7)
      [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 |];
  ]

(** Test cases for take function *)
let test_take =
  [
    test "take from list" (take 2 [ 1; 2; 3; 4; 5 ]) [ 1; 2 ];
    test "take more than list length" (take 10 [ 1; 2; 3 ]) [ 1; 2; 3 ];
    test "take zero elements" (take 0 [ 1; 2; 3; 4; 5 ]) [];
    test "take from empty list" (take 3 []) [];
    test "take from string list"
      (take 2 [ "apple"; "banana"; "cherry"; "date" ])
      [ "apple"; "banana" ];
    test "take single element" (take 1 [ 7; 8; 9; 10 ]) [ 7 ];
  ]

(** Test cases for create_batches function *)
let test_create_batches =
  [
    test "create_batches basic"
      (create_batches [ 1; 2; 3; 4; 5 ] 2)
      [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ];
    test "create_batches single element per batch"
      (create_batches [ 1; 2; 3; 4; 5 ] 1)
      [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ];
    test "create_batches large batch size"
      (create_batches [ 1; 2; 3 ] 5)
      [ [ 1; 2; 3 ] ];
    test "create_batches empty list" (create_batches [] 3) [];
    test "create_batches batch size equals list size"
      (create_batches [ 1; 2; 3; 4 ] 4)
      [ [ 1; 2; 3; 4 ] ];
    test "create_batches uneven last batch"
      (create_batches [ 1; 2; 3; 4; 5; 6 ] 4)
      [ [ 1; 2; 3; 4 ]; [ 5; 6 ] ];
  ]

(** Test cases for percentile function *)
let test_percentile =
  [
    test "percentile basic" (percentile 50. [ 1; 2; 3; 4; 5 ]) 3;
    test "percentile different percentile" (percentile 25. [ 1; 2; 3; 4; 5 ]) 2;
    test "percentile larger list"
      (percentile 50. (List.init 20 (fun i -> i)))
      10;
    test "percentile repeated values" (percentile 75. [ 1; 2; 2; 2; 3; 4 ]) 3;
    test "percentile non-uniform distribution"
      (percentile 30. [ 1; 3; 7; 10; 15 ])
      3;
    test "percentile 0th percentile" (percentile 0. [ 5; 4; 3; 2; 1 ]) 1;
    test "percentile 100th percentile" (percentile 100. [ 5; 4; 3; 2; 1 ]) 5;
  ]

(** Test cases for create_vector function *)
let test_create_vector =
  [
    test "create_vector size 3" (create_vector 3) [| 0.0; 0.0; 0.0 |];
    test "create_vector size 0" (create_vector 0) [||];
    test "create_vector negative size"
      (try
         let _ = create_vector (-1) in
         false
       with Invalid_argument _ -> true)
      true;
    test "create_vector large size" (Array.length (create_vector 1000)) 1000;
  ]

(** Test cases for vocab_size function *)
let test_vocab_size =
  [
    test "vocab_size empty" (vocab_size (Hashtbl.create 0)) 0;
    test "vocab_size non-empty"
      (let ht = Hashtbl.create 5 in
       Hashtbl.add ht "word1" 1;
       Hashtbl.add ht "word2" 2;
       vocab_size ht)
      2;
    test "vocab_size large vocabulary"
      (let ht = Hashtbl.create 1000 in
       for i = 1 to 1000 do
         Hashtbl.add ht (string_of_int i) i
       done;
       vocab_size ht)
      1000;
    test "vocab_size with duplicate values"
      (let ht = Hashtbl.create 10 in
       Hashtbl.add ht "word1" 1;
       Hashtbl.add ht "word2" 1;
       vocab_size ht)
      2;
    test "vocab_size randomly populated"
      (let ht = Hashtbl.create 100 in
       for i = 1 to 100 do
         Hashtbl.add ht (string_of_int i) (Random.int 100)
       done;
       vocab_size ht)
      100;
    test "vocab_size all keys no values"
      (let ht = Hashtbl.create 5 in
       List.iter (fun key -> Hashtbl.add ht key ()) [ "a"; "b"; "c"; "d"; "e" ];
       vocab_size ht)
      5;
  ]

(** Test cases for learning_rate_decay function *)
let test_learning_rate_decay =
  [
    test "learning_rate_decay basic" (learning_rate_decay 0.01 10 20) 0.005;
    test "learning_rate_decay early epoch"
      (learning_rate_decay 0.01 1 20)
      0.0095;
    test "learning_rate_decay last epoch" (learning_rate_decay 0.01 20 20) 0.0;
    test "learning_rate_decay high initial rate"
      (learning_rate_decay 0.1 10 20)
      0.05;
    test "learning_rate_decay max epochs 1" (learning_rate_decay 0.01 1 1) 0.0;
  ]

let suite =
  "test_suite"
  >::: List.flatten
         [
           data_test;
           test_randomize_list;
           test_tokenize_sentence;
           test_tokenize_nested_list;
           test_randomize_nested_list;
           test_count_word_frequencies;
           test_build_vocabulary;
           test_one_hot_encode;
           test_padding_token;
           test_take;
           test_create_batches;
           test_percentile;
           test_create_vector;
           test_vocab_size;
           test_learning_rate_decay;
         ]

let () = run_test_tt_main suite
