open Bogue
module W = Widget
module L = Layout
module T = Trigger

(* The main file for the demo Chatbot we are making. For now all items
   will be in this file, further along the line we will be seperating the
   action events and what happens when the Chatbot replies to the user in
   another file. The Chatbot utilizes the features from the Bogue class that
   can be found on this website
   https://sanette.github.io/bogue/Bogue.html*)

(* Images of the robot, hearts, and circles. Setup for the layouts are
   also instantinated to display on the Bogue window *)

let robot_image = W.image ~w:300 ~h:400 "robot.png"
let heart_1 = W.image ~w:50 ~h:80 "heart.png"
let heart_2 = W.image ~w:50 ~h:80 "heart.png"
let heart_3 = W.image ~w:50 ~h:80 "heart.png"
let heart_4 = W.image ~w:50 ~h:80 "heart.png"
let heart_5 = W.image ~w:50 ~h:80 "heart.png"
let heart_6 = W.image ~w:50 ~h:80 "heart.png"
let heart_7 = W.image ~w:50 ~h:80 "heart.png"
let heart_8 = W.image ~w:50 ~h:80 "heart.png"
let heart_9 = W.image ~w:50 ~h:80 "heart.png"
let heart_10 = W.image ~w:50 ~h:80 "heart.png"
let circle1 = W.image ~w:120 ~h:160 "Circle.png"
let circle2 = W.image ~w:120 ~h:160 "Circle.png"
let circle3 = W.image ~w:120 ~h:160 "Circle.png"
let circle4 = W.image ~w:120 ~h:160 "Circle.png"
let circle5 = W.image ~w:120 ~h:160 "Circle.png"
let circle6 = W.image ~w:120 ~h:160 "Circle.png"
let circle7 = W.image ~w:120 ~h:160 "Circle.png"
let big_circle = W.image ~w:400 ~h:350 "Circle.png"
let big_circle_2 = W.image ~w:300 ~h:200 "Circle.png"
let big_circle_3 = W.image ~w:100 ~h:100 "Circle.png"
let circle8 = W.image ~w:120 ~h:160 "Circle.png"
let circle9 = W.image ~w:120 ~h:160 "Circle.png"
let circle10 = W.image ~w:120 ~h:160 "Circle.png"
let circle11 = W.image ~w:120 ~h:160 "Circle.png"
let circle12 = W.image ~w:120 ~h:160 "Circle.png"
let circle13 = W.image ~w:120 ~h:160 "Circle.png"
let circle14 = W.image ~w:120 ~h:160 "Circle.png"
let big_circle_4 = W.image ~w:400 ~h:350 "Circle.png"
let big_circle_5 = W.image ~w:300 ~h:200 "Circle.png"
let big_circle_6 = W.image ~w:100 ~h:100 "Circle.png"
let layout_1 = L.resident ~x:1118 ~y:105 circle1
let layout_2 = L.resident ~x:1100 ~y:0 circle2
let layout_3 = L.resident ~x:1167 ~y:35 circle3
let layout_4 = L.resident ~x:1200 ~y:150 circle4
let layout_5 = L.resident ~x:1267 ~y:185 circle5
let layout_6 = L.resident ~x:1300 ~y:300 circle6
let layout_7 = L.resident ~x:1218 ~y:235 circle7
let layout_8 = L.resident ~x:162 ~y:105 circle8
let layout_9 = L.resident ~x:180 ~y:0 circle9
let layout_10 = L.resident ~x:113 ~y:35 circle10
let layout_11 = L.resident ~x:80 ~y:150 circle11
let layout_12 = L.resident ~x:13 ~y:185 circle12
let layout_13 = L.resident ~x:(-20) ~y:300 circle13
let layout_14 = L.resident ~x:62 ~y:235 circle14
let layout_15 = L.resident ~x:1200 ~y:0 big_circle
let layout_16 = L.resident ~x:1200 ~y:0 big_circle_2
let layout_17 = L.resident ~x:1200 ~y:0 big_circle_3
let layout_18 = L.resident ~x:(-200) ~y:0 big_circle_4
let layout_19 = L.resident ~x:(-200) ~y:0 big_circle_5
let layout_20 = L.resident ~x:100 ~y:0 big_circle_6
let displayed_heart_1 = L.resident ~x:400 ~y:70 heart_1
let displayed_heart_2 = L.resident ~x:200 ~y:600 heart_2
let displayed_heart_3 = L.resident ~x:900 ~y:70 heart_3
let displayed_heart_4 = L.resident ~x:1300 ~y:700 heart_4
let displayed_heart_5 = L.resident ~x:1000 ~y:600 heart_5
let displayed_heart_6 = L.resident ~x:300 ~y:900 heart_6
let displayed_heart_7 = L.resident ~x:1150 ~y:900 heart_7
let displayed_heart_8 = L.resident ~x:50 ~y:920 heart_8
let displayed_heart_9 = L.resident ~x:1100 ~y:400 heart_9
let displayed_heart_10 = L.resident ~x:300 ~y:400 heart_10

(* Identfies simple text within text layout, for this example if we type in
   "Hello!" the chatbot will say Hello! in return.*)
let update_text (text_input : string) : string =
  match text_input with
  | "Hello!" -> "Hi, how are you!"
  | _ -> "Please type something!"

let text_input_box = W.text_input ~size:25 ~prompt:"Type and chat" ()
let header = W.label ~size:30 ~fg:(255, 0, 0, 0) "Waiting for your response"

(* Main application of running the window. Objects that the window should start
   outwith the inlcusion of initial features. This include the hearts, robot,
   and layouts nescesary when opening the initial frame of the RizzBot. *)

let () =
  let rizzbot_name = W.label ~size:30 ~fg:(255, 0, 0, 0) "Rizzbot" in
  let header =
    W.label ~size:30 ~fg:(255, 0, 0, 0) "Waiting for your response"
  in
  let robot_pos = L.resident ~x:560 ~y:350 robot_image in
  let name_layout = L.resident ~x:500 ~y:150 rizzbot_name in
  let header_layout = L.resident ~x:500 ~y:250 header in
  let text_input_box = W.text_input ~size:25 ~prompt:"Type and chat" () in
  let text_layout = L.resident ~x:550 ~y:900 ~w:500 ~h:100 text_input_box in
  let input_display =
    W.label ~size:20 ~fg:(0, 0, 0, 255) "Your input will appear here"
  in
  (* Define a reference to store the user input *)
  let rnn_input = ref "" in
  let button_action _ =
    let input_text = W.get_text text_input_box in
    W.set_text header (update_text input_text);
    (* Update the header with the response *)
    W.set_text input_display
      input_text (* Update the input_display with the input text *)
  in

  (* Button widget *)
  let button = W.button ~action:button_action "Insert chat" in
  let button_layout = L.resident button ~x:750 ~y:900 ~w:100 ~h:50 in
  let input_display_layout =
    L.resident ~x:550 ~y:850 ~w:500 ~h:50 input_display
  in

  (* After the GUI is closed, you can use rnn_input *)
  print_endline !rnn_input;

  (* This will print the last input received before the GUI was closed *)
  let full_layout =
    L.superpose ~w:1400 ~h:1250
      [
        name_layout;
        robot_pos;
        layout_1;
        layout_2;
        layout_3;
        layout_4;
        layout_5;
        layout_6;
        layout_7;
        layout_8;
        layout_9;
        layout_10;
        layout_11;
        layout_12;
        layout_13;
        layout_14;
        layout_15;
        layout_16;
        layout_17;
        layout_18;
        layout_19;
        layout_20;
        displayed_heart_1;
        displayed_heart_2;
        displayed_heart_3;
        displayed_heart_4;
        displayed_heart_5;
        displayed_heart_6;
        displayed_heart_7;
        displayed_heart_9;
        displayed_heart_10;
        displayed_heart_8;
        text_layout;
        button_layout;
        header_layout;
        input_display_layout;
      ]
  in

  let board = Bogue.of_layout full_layout in
  Bogue.run board
