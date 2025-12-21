open! Core
open! Hardcaml
open! Signal

let num_bits = 64 (* integer size for calculations *)
let addr_bits = 8 (* RAM size will be 2^(addr_bits) *)
let fifo_depth = 65536

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
    | Part2_setup
    | Part2_read
    | Part2_write
    | Read_stack
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
  let%hw_var sum2 = Variable.reg spec ~width:num_bits in
  let%hw_var max_joltage = Variable.reg spec ~width:num_bits in
  let%hw_var joltage = Variable.wire ~default:(zero num_bits) () in
  let%hw_var max_digit = Variable.reg spec ~width:num_bits in

  (* FIFO *)
  let%hw_var fifo_rd = Variable.wire ~default:gnd () in
  let%tydi { q = fifo_front; full = fifo_full; empty = fifo_empty; _ } =
    Fifo.create
      ~showahead:true
      ~scope:(Scope.sub_scope scope "fifo")
      ~capacity:fifo_depth
      ~overflow_check:true
      ~underflow_check:true
      ~clock
      ~clear
      ~wr:data_in_valid
      ~d:data_in
      ~rd:fifo_rd.value
      ()
  in
  let%hw fifo_full in
  let%hw fifo_empty in
  let%hw fifo_front in

  (* RAM *)
  let%hw_var rd_addr = Variable.reg spec ~width:addr_bits in
  let%hw_var rd_enable = Variable.wire ~default:gnd () in

  let%hw_var wr_addr = Variable.reg spec ~width:addr_bits in
  let%hw_var wr_enable = Variable.wire ~default:gnd () in
  let%hw_var wr_data = Variable.wire ~default:(zero num_bits) () in
  let mem =
    Hardcaml.Ram.create
      ~collision_mode:Read_before_write
      ~size:(Int.pow 2 addr_bits)
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
                ; sum2 <--. 0
                ; max_joltage <-- zero num_bits
                ; joltage <-- zero num_bits
                ; max_digit <-- zero num_bits
                ; wr_addr <--. 0
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid
              [ when_ fifo_full [ sm.set_next Part2_setup ] ]
            ; when_ finish [ sm.set_next Part2_setup ]
            ] )
        ; ( Part2_setup
            , [ rd_addr <-- ones addr_bits
              ; wr_addr <-- ones addr_bits
              ; sm.set_next Part2_read
            ] )
        ; ( Part2_read
            , [ if_     (fifo_empty |: (fifo_front ==:. 0)) [ sm.set_next Done ]
                @@ elif (fifo_front ==:. Char.to_int '\n') (* Next line *)
                        [ sm.set_next Read_stack
                        ; rd_addr <--. 0
                        ; rd_enable <-- vdd
                        ; max_joltage <--. 0
                        ; fifo_rd <-- vdd
                        ]
                @@      [ rd_enable <-- vdd
                        ; rd_addr <-- rd_addr.value +:. 1
                        ; wr_addr <-- wr_addr.value +:. 1
                        ; sm.set_next Part2_write
                        ]
            ] )
        ; ( Part2_write
            , [ when_ (rd_addr.value <=+. 12)
                [ wr_data <-- uresize ~width:num_bits fifo_front -:. Char.to_int '0'
                ; wr_enable <-- vdd
                ]
              ; fifo_rd <-- vdd
              ; sm.set_next Part2_read
            ] )
        ; ( Read_stack
            , [ max_joltage <-- uresize ~width:num_bits (max_joltage.value *: of_string "4'd10") +: mem.(0)
              ; when_ (rd_addr.value >+. 12) [ sum2 <-- sum2.value +: max_joltage.value ; sm.set_next Part2_setup ]
              ; rd_addr <-- rd_addr.value +:. 1
              ; rd_enable <-- vdd
            ] )
        ; ( Done
          , [ answer1 <--. 0
            ; answer1_valid <-- vdd
            ; answer2 <-- sum2.value
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
