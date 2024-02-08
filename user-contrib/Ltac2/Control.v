(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

Require Import Ltac2.Init.
Require Ltac2.Message.

(** Panic *)

Ltac2 @ external throw : exn -> 'a := "coq-core.plugins.ltac2" "throw".
(** Fatal exception throwing. This does not induce backtracking. *)

(** Generic backtracking control *)

Ltac2 @ external zero : exn -> 'a := "coq-core.plugins.ltac2" "zero".
Ltac2 @ external plus : (unit -> 'a) -> (exn -> 'a) -> 'a := "coq-core.plugins.ltac2" "plus".
Ltac2 @ external once : (unit -> 'a) -> 'a := "coq-core.plugins.ltac2" "once".
Ltac2 @ external dispatch : (unit -> unit) list -> unit := "coq-core.plugins.ltac2" "dispatch".
Ltac2 @ external extend : (unit -> unit) list -> (unit -> unit) -> (unit -> unit) list -> unit := "coq-core.plugins.ltac2" "extend".
Ltac2 @ external enter : (unit -> unit) -> unit := "coq-core.plugins.ltac2" "enter".
Ltac2 @ external case : (unit -> 'a) -> ('a * (exn -> 'a)) result := "coq-core.plugins.ltac2" "case".

Ltac2 once_plus (run : unit -> 'a) (handle : exn -> 'a) : 'a :=
  once (fun () => plus run handle).

(** Proof state manipulation *)

Ltac2 @ external focus : int -> int -> (unit -> 'a) -> 'a := "coq-core.plugins.ltac2" "focus".
Ltac2 @ external shelve : unit -> unit := "coq-core.plugins.ltac2" "shelve".
Ltac2 @ external shelve_unifiable : unit -> unit := "coq-core.plugins.ltac2" "shelve_unifiable".

Ltac2 @ external new_goal : evar -> unit := "coq-core.plugins.ltac2" "new_goal".
(** Adds the given evar to the list of goals as the last one. If it is
    already defined in the current state, don't do anything. Panics if the
    evar is not in the current state. *)

Ltac2 @ external unshelve : (unit -> 'a) -> 'a := "coq-core.plugins.ltac2" "unshelve".
(** Runs the closure, then unshelves existential variables added to the
    shelf by its execution, prepending them to the current goal.
    Returns the value produced by the closure. *)

Ltac2 @ external progress : (unit -> 'a) -> 'a := "coq-core.plugins.ltac2" "progress".

(** Goal inspection *)

Ltac2 @ external goal : unit -> constr := "coq-core.plugins.ltac2" "goal".
(** Panics if there is not exactly one goal under focus. Otherwise returns
    the conclusion of this goal. *)

Ltac2 @ external hyp : ident -> constr := "coq-core.plugins.ltac2" "hyp".
(** Panics if there is more than one goal under focus. If there is no
    goal under focus, looks for the section variable with the given name.
    If there is one, looks for the hypothesis with the given name. *)

Ltac2 @ external hyps : unit -> (ident * constr option * constr) list := "coq-core.plugins.ltac2" "hyps".
(** Panics if there is more than one goal under focus. If there is no
    goal under focus, returns the list of section variables.
    If there is one, returns the list of hypotheses. In both cases, the
    list is ordered with rightmost values being last introduced. *)

(** Refinement *)

Ltac2 @ external refine : (unit -> constr) -> unit := "coq-core.plugins.ltac2" "refine".

(** Evars *)

Ltac2 @ external with_holes : (unit -> 'a) -> ('a -> 'b) -> 'b := "coq-core.plugins.ltac2" "with_holes".
(** [with_holes x f] evaluates [x], then apply [f] to the result, and fails if
    all evars generated by the call to [x] have not been solved when [f]
    returns. *)

(** Misc *)

Ltac2 @ external time : string option -> (unit -> 'a) -> 'a := "coq-core.plugins.ltac2" "time".
(** Displays the time taken by a tactic to evaluate. *)

Ltac2 @ external abstract : ident option -> (unit -> unit) -> unit := "coq-core.plugins.ltac2" "abstract".
(** Abstract a subgoal. *)

Ltac2 @ external check_interrupt : unit -> unit := "coq-core.plugins.ltac2" "check_interrupt".
(** For internal use. *)

(** Assertions throwing exceptions and short form throws *)

Ltac2 throw_invalid_argument (msg : string) :=
  Control.throw (Invalid_argument (Some (Message.of_string msg))).

Ltac2 throw_out_of_bounds (msg : string) :=
  Control.throw (Out_of_bounds (Some (Message.of_string msg))).

Ltac2 assert_valid_argument (msg : string) (test : bool) :=
  match test with
  | true => ()
  | false => throw_invalid_argument msg
  end.

Ltac2 assert_bounds (msg : string) (test : bool) :=
  match test with
  | true => ()
  | false => throw_out_of_bounds msg
  end.

Ltac2 assert_true b :=
  if b then () else throw Assertion_failure.

Ltac2 assert_false b :=
  if b then throw Assertion_failure else ().

(** Short form backtracks *)

Ltac2 backtrack_tactic_failure (msg : string) :=
  Control.zero (Tactic_failure (Some (Message.of_string msg))).

(** Backtraces. *)

(** [throw_bt info e] is similar to [throw e], but raises [e] with the
    backtrace represented by [info]. *)
Ltac2 @ external throw_bt : exn -> exninfo -> 'a :=
  "coq-core.plugins.ltac2" "throw_bt".

(** [zero_bt info e] is similar to [zero e], but raises [e] with the
    backtrace represented by [info]. *)
Ltac2 @ external zero_bt : exn -> exninfo -> 'a :=
  "coq-core.plugins.ltac2" "zero_bt".

(** [plus_bt run handle] is similar to [plus run handle] (up to the type
    missmatch for [handle]), but it calls [handle] with an extra argument
    representing the backtrace at the point of the exception. The [handle]
    function can thus decide to re-attach that backtrace when using the
    [throw_bt] or [zero_bt] functions. *)
Ltac2 @ external plus_bt : (unit -> 'a) -> (exn -> exninfo -> 'a) -> 'a :=
  "coq-core.plugins.ltac2" "plus_bt".

(** [once_plus_bt run handle] is a non-backtracking variant of [once_plus]
    that has backtrace support similar to that of [plus_bt]. *)
Ltac2 once_plus_bt (run : unit -> 'a) (handle : exn -> exninfo -> 'a) : 'a :=
  once (fun _ => plus_bt run handle).

Ltac2 @ external clear_err_info : err -> err :=
  "coq-core.plugins.ltac2" "clear_err_info".

Ltac2 clear_exn_info (e : exn) : exn :=
  match e with
  | Init.Internal err => Init.Internal (clear_err_info err)
  | e => e
  end.

(** Timeout. *)

(** [timeout t thunk] calls [thunk ()] with a timeout of [t] seconds. *)
Ltac2 @ external timeout : int -> (unit -> 'a) -> 'a :=
  "coq-core.plugins.ltac2" "timeout".

(** [timeoutf t thunk] calls [thunk ()] with a timeout of [t] seconds. *)
Ltac2 @ external timeoutf : float -> (unit -> 'a) -> 'a :=
  "coq-core.plugins.ltac2" "timeoutf".
