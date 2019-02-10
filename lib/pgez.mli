(** [Pgez] -- Eazy wrapper of [postgresql-ocaml].

    You can send query params and can retrieve result columns
    with type safe manner.

    This library intended to use with {!Core}.
*)

open Core_kernel

module Value : sig
  type t =
    [ `Bool of bool
    | `Bytes of Cstruct.t
    | `Date of Date.t
    | `Float of float
    | `Int of int
    | `Int64 of int64
    | `Ipaddr of Ipaddr.t
    | `Ipaddr_prefix of Ipaddr.Prefix.t
    | `Json of Yojson.Safe.t
    | `Macaddr of Macaddr.t
    | `Null
    | `String of string
    | `Time of Time.t
    | `Time_ofday of Time.Ofday.t
    | `Time_span of Time.Span.t
    | `Uuid of Uuidm.t

    | `Otherwise of int * string ]
  (** Value of column. *)

  val sexp_of_t: t -> Sexp.t
end

val exec:
  db:Postgresql.connection ->
  ?expect:Postgresql.result_status list ->
  ?params:Value.t array ->
  string ->
  Value.t array list
(** [exec ~db ?expect ?params query] execute [query] and retrieves
    all results. *)

val with_transaction:
  db:Postgresql.connection ->
  f:(unit -> ('a, [> `Pgez_transaction_abort of exn ] as 'b) Result.t) ->
  ('a, 'b) Result.t
(** [with_transaction ~db ~f] starts [f] in [db] transaction.
    The started transaction will commit if [f] returns [Ok _],
    or will rollback if [f] returns [Error _] or raise exception.

    [f] may be called multiple times when transaction serialization.
    You should avoid side effect within [f]. *)
