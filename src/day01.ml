(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let num_bits = 16

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits num_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      answer1 : 'a With_valid.t [@bits num_bits]
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
    (* Note that the state machine defaults to initializing to the first state *)
    State_machine.create (module States) spec
  in
  (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
     will show up in waveforms. The [_var] version is used when working with the Always
     DSL. *)
  let%hw_var count_end0 = Variable.reg spec ~width:num_bits in
  let%hw_var count0 = Variable.reg spec ~width:num_bits in
  let%hw_var dial = Variable.reg spec ~width:num_bits in
  let%hw_var clicks = Variable.reg spec ~width:num_bits in
  let%hw_var dir = Variable.reg spec ~width:1 in
  (* 0:left, 1:right *)

  (* FIFO *)
  let%hw_var fifo_rd = Variable.wire ~default:gnd () in
  let%tydi { q = fifo_front; full = fifo_full; empty = fifo_empty; _ } =
    Fifo.create
      ~showahead:true
      ~scope:(Scope.sub_scope scope "fifo")
      ~capacity:5000
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
  (* We don't need to name the variable here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let answer1 = Variable.wire ~default:(zero num_bits) () in
  let answer1_valid = Variable.wire ~default:gnd () in
  let answer2 = Variable.wire ~default:(zero num_bits) () in
  let answer2_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ count_end0 <-- zero num_bits
                ; count0 <-- zero num_bits
                ; dial <-- of_signed_int ~width:16 50
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid [ when_ fifo_full [ sm.set_next Looping ] ]
            ; when_ finish [ sm.set_next Looping ]
            ] )
        ; ( Looping
          , [ if_ (fifo_empty &: (clicks.value ==:. 0)) [ sm.set_next Done ]
              @@ elif
                   (clicks.value ==:. 0) (* Read next input from FIFO *)
                   [ when_ (dial.value ==:. 0) [ count_end0 <-- count_end0.value +:. 1 ]
                   ; if_
                       (fifo_front <+. 0)
                       [ dir <-- gnd; clicks <-- zero num_bits -: fifo_front ]
                       [ dir <-- vdd; clicks <-- fifo_front ]
                   ; fifo_rd <--. 1
                   ]
              @@ [ clicks <-- clicks.value -:. 1
                 ; if_
                     dir.value
                     [ if_
                         (dial.value ==:. 99)
                         [ dial <--. 0; count0 <-- count0.value +:. 1 ]
                         [ dial <-- dial.value +:. 1 ]
                     ]
                     [ when_ (dial.value ==:. 1) [ count0 <-- count0.value +:. 1 ]
                     ; if_
                         (dial.value ==:. 0)
                         [ dial <--. 99 ]
                         [ dial <-- dial.value -:. 1 ]
                     ]
                 ]
            ] )
        ; ( Done
          , [ answer1 <-- count_end0.value
            ; answer1_valid <-- vdd
            ; answer2 <-- count0.value
            ; answer2_valid <-- vdd
            ; when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { answer1 = { value = answer1.value; valid = answer1_valid.value }
  ; answer2 = { value = answer2.value; valid = answer2_valid.value }
  }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day01" create
;;
