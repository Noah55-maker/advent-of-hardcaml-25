open! Core
open! Hardcaml
open! Signal

let num_bits = 16

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits 8]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { answer1 : 'a With_valid.t [@bits num_bits]
    ; answer2 : 'a With_valid.t [@bits num_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Looping
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in

  let%hw_var sum = Variable.reg spec ~width:num_bits in
  let%hw_var max_joltage = Variable.reg spec ~width:num_bits in
  let%hw_var joltage = Variable.wire ~default:(zero num_bits) () in
  let%hw_var digit = Variable.wire ~default:(zero num_bits) () in
  let%hw_var max_digit = Variable.reg spec ~width:num_bits in

  (* RAM *)
  let%hw_var rd_addr = Variable.reg spec ~width:num_bits in
  let%hw_var rd_enable = Variable.wire ~default:gnd () in

  let%hw_var wr_addr = Variable.reg spec ~width:num_bits in
  let%hw_var wr_enable = Variable.wire ~default:gnd () in
  let%hw_var wr_data = Variable.wire ~default:(zero num_bits) () in
  let mem =
    Hardcaml.Ram.create
      ~collision_mode:Read_before_write
      ~size:65536
      ~write_ports:
        [| { write_clock = clock
            ; write_address = wr_addr.value
            ; write_enable = wr_enable.value
            ; write_data = wr_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = clock
           ; read_address = rd_addr.value
           ; read_enable = rd_enable.value
           }
        |]
      ()
  in

  let answer1 = Variable.wire ~default:(zero num_bits) () in
  let answer1_valid = Variable.wire ~default:gnd () in
  let answer2 = Variable.wire ~default:(zero num_bits) () in
  let answer2_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ sum <-- zero num_bits
                ; max_joltage <-- zero num_bits
                ; joltage <-- zero num_bits
                ; max_digit <-- zero num_bits
                ; sm.set_next Accepting_inputs
                ; wr_addr <--. 0
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid
              [ if_ (wr_addr.value ==:. 65535)
                  [ sm.set_next Looping ]
                  [ wr_data <-- uresize ~width:16 data_in
                  ; wr_enable <-- vdd
                  ; wr_addr <-- wr_addr.value +:. 1
                  ]
              ]
            ; when_ finish [ sm.set_next Looping ]
            ] )
        ; ( Looping
          , [ if_ (rd_addr.value >: wr_addr.value) [ sm.set_next Done ]
                (* Read next input from RAM *)
                [ if_ (mem.(0) ==:. Char.to_int '\n')
                    [ sum <-- sum.value +: max_joltage.value ; max_joltage <--. 0 ; max_digit <--. 0 ]
                @@ elif (mem.(0) ==:. 0) []
                @@  [ digit <-- mem.(0) -:. Char.to_int '0'
                    ; joltage <-- (uresize ~width:num_bits (max_digit.value *: (of_string "4'd10"))) +: digit.value
                    ; when_ (joltage.value >: max_joltage.value) [ max_joltage <-- joltage.value ]
                    ; when_ (max_digit.value <: digit.value) [ max_digit <-- digit.value ]
                    ]
                ; rd_enable <-- vdd
                ; rd_addr <-- rd_addr.value +:. 1
                ]
            ] )
        ; ( Done
          , [ answer1 <-- sum.value
            ; answer1_valid <-- vdd
            ; answer2 <-- sum.value
            ; answer2_valid <-- vdd
            ; when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  { answer1 = { value = answer1.value; valid = answer1_valid.value }
  ; answer2 = { value = answer2.value; valid = answer2_valid.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day03" create
;;
