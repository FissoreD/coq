(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Untyped intermediate terms *)

(** [glob_constr] comes after [constr_expr] and before [constr].

   Resolution of names, insertion of implicit arguments placeholder,
   and notations are done, but coercions, inference of implicit
   arguments and pattern-matching compilation are not. *)

open Names

type existential_name = Id.t

(** Sorts *)

type glob_qvar =
  | GLocalQVar of lname
  | GQVar of Sorts.QVar.t
  | GRawQVar of Sorts.QVar.t (* hack for funind *)

type glob_relevance =
  | GRelevant | GIrrelevant
  | GRelevanceVar of glob_qvar

type glob_quality =
  | GQConstant of Sorts.Quality.constant
  | GQualVar of glob_qvar

type glob_sort_name =
  | GSProp (** representation of [SProp] literal *)
  | GProp (** representation of [Prop] level *)
  | GSet  (** representation of [Set] level *)
  | GUniv of Univ.Level.t
  | GLocalUniv of lident (** Locally bound universes (may also be nonstrict declaration) *)
  | GRawUniv of Univ.Level.t
  (** Hack for funind, DO NOT USE

      Note that producing the similar Constrexpr.CRawType for printing
      is OK, just don't try to reinterp it. *)

type 'a glob_sort_gen =
  | UAnonymous of { rigid : UState.rigid } (** not rigid = unifiable by minimization *)
  | UNamed of 'a

(** levels, occurring in universe instances *)
type glob_level = glob_sort_name glob_sort_gen

type glob_instance = glob_quality list * glob_level list

(** sort expressions *)
type glob_sort = (glob_qvar option * (glob_sort_name * int) list glob_sort_gen)

type glob_constraint = glob_sort_name * Univ.constraint_type * glob_sort_name

type glob_recarg = int option

and glob_fix_kind =
  | GFix of (glob_recarg array * int)
  | GCoFix of int

(**  The kind of patterns that occurs in "match ... with ... end"

     locs here refers to the ident's location, not whole pat *)
type 'a cases_pattern_r =
  | PatVar  of Name.t
  | PatCstr of constructor * 'a cases_pattern_g list * Name.t
      (** [PatCstr(p,C,l,x)] = "|'C' 'l' as 'x'" *)
and 'a cases_pattern_g = ('a cases_pattern_r, 'a) DAst.t

type cases_pattern = [ `any ] cases_pattern_g

type binding_kind = Explicit | MaxImplicit | NonMaxImplicit

type relevance_info = glob_relevance option

type glob_evar_kind = Evar_kinds.glob_evar_kind =
  | GImplicitArg of GlobRef.t * (int * Id.t) * bool (** Force inference *)
  | GBinderType of Name.t
  | GNamedHole of bool (* fresh? *) * Id.t (* coming from some ?[id] syntax *)
  | GQuestionMark of Evar_kinds.question_mark
  | GCasesType
  | GInternalHole
  | GImpossibleCase

(** Representation of an internalized (or in other words globalized) term. *)
type 'a glob_constr_r =
  | GRef of GlobRef.t * glob_instance option
      (** An identifier that represents a reference to an object defined
          either in the (global) environment or in the (local) context. *)
  | GVar of Id.t
      (** An identifier that cannot be regarded as "GRef".
          Bound variables are typically represented this way. *)
  | GEvar   of existential_name CAst.t * (lident * 'a glob_constr_g) list
  | GPatVar of Evar_kinds.matching_var_kind (** Used for patterns only *)
  | GApp    of 'a glob_constr_g * 'a glob_constr_g list
  | GLambda of Name.t * relevance_info * binding_kind *  'a glob_constr_g * 'a glob_constr_g
  | GProd   of Name.t * relevance_info * binding_kind * 'a glob_constr_g * 'a glob_constr_g
  | GLetIn  of Name.t * relevance_info * 'a glob_constr_g * 'a glob_constr_g option * 'a glob_constr_g
  | GCases  of Constr.case_style * 'a glob_constr_g option * 'a tomatch_tuples_g * 'a cases_clauses_g
      (** [GCases(style,r,tur,cc)] = "match 'tur' return 'r' with 'cc'" (in [MatchStyle]) *)
  | GLetTuple of Name.t list * (Name.t * 'a glob_constr_g option) * 'a glob_constr_g * 'a glob_constr_g
  | GIf   of 'a glob_constr_g * (Name.t * 'a glob_constr_g option) * 'a glob_constr_g * 'a glob_constr_g
  | GRec  of glob_fix_kind * Id.t array * 'a glob_decl_g list array *
             'a glob_constr_g array * 'a glob_constr_g array
  | GSort of glob_sort
  | GHole of glob_evar_kind
  | GGenarg of Genarg.glob_generic_argument
  | GCast of 'a glob_constr_g * Constr.cast_kind option * 'a glob_constr_g
  | GProj of (Constant.t * glob_instance option) * 'a glob_constr_g list * 'a glob_constr_g
  | GInt of Uint63.t
  | GFloat of Float64.t
  | GString of Pstring.t
  | GArray of glob_instance option * 'a glob_constr_g array * 'a glob_constr_g * 'a glob_constr_g
and 'a glob_constr_g = ('a glob_constr_r, 'a) DAst.t

and 'a glob_decl_g = Name.t * relevance_info * binding_kind * 'a glob_constr_g option * 'a glob_constr_g

and 'a predicate_pattern_g =
    Name.t * (inductive * Name.t list) CAst.t option
      (** [(na,id)] = "as 'na' in 'id'" where if [id] is [Some(l,I,k,args)]. *)

and 'a tomatch_tuple_g = ('a glob_constr_g * 'a predicate_pattern_g)

and 'a tomatch_tuples_g = 'a tomatch_tuple_g list

and 'a cases_clause_g = (Id.t list * 'a cases_pattern_g list * 'a glob_constr_g) CAst.t
(** [(il,cl,t)] = "|'cl' => 't'". Precondition: the free variables
    of [t] are members of [il]. *)

and 'a cases_clauses_g = 'a cases_clause_g list

type glob_constr = [ `any ] glob_constr_g
type tomatch_tuple = [ `any ] tomatch_tuple_g
type tomatch_tuples = [ `any ] tomatch_tuples_g
type cases_clause = [ `any ] cases_clause_g
type cases_clauses = [ `any ] cases_clauses_g
type glob_decl = [ `any ] glob_decl_g
type predicate_pattern = [ `any ] predicate_pattern_g

type any_glob_constr = AnyGlobConstr : 'r glob_constr_g -> any_glob_constr

type 'a disjunctive_cases_clause_g = (Id.t list * 'a cases_pattern_g list list * 'a glob_constr_g) CAst.t
type 'a disjunctive_cases_clauses_g = 'a disjunctive_cases_clause_g list
type 'a cases_pattern_disjunction_g = 'a cases_pattern_g list

type disjunctive_cases_clause = [ `any ] disjunctive_cases_clause_g
type disjunctive_cases_clauses = [ `any ] disjunctive_cases_clauses_g
type cases_pattern_disjunction = [ `any ] cases_pattern_disjunction_g

type 'a extended_glob_local_binder_r =
  | GLocalAssum   of Name.t * relevance_info * binding_kind * 'a glob_constr_g
  | GLocalDef     of Name.t * relevance_info * 'a glob_constr_g * 'a glob_constr_g option
  | GLocalPattern of ('a cases_pattern_disjunction_g * Id.t list) * Id.t * binding_kind * 'a glob_constr_g
and 'a extended_glob_local_binder_g = ('a extended_glob_local_binder_r, 'a) DAst.t

type extended_glob_local_binder = [ `any ] extended_glob_local_binder_g
