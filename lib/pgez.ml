open Core_kernel

module Value = struct
  type t = [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Int64 of int64
    | `Float of float
    | `String of string
    | `Time of Time.t
    | `Date of Date.t
    | `Time_ofday of Time.Ofday.t
    | `Time_span of Time.Span.t
    | `Uuid of Uuidm.t
    | `Json of Yojson.Safe.t
    | `Bytes of Cstruct.t
    | `Ipaddr of Ipaddr.t
    | `Ipaddr_prefix of Ipaddr.Prefix.t
    | `Macaddr of Macaddr.t
    | `Otherwise of int * string
  ]

  let to_internal : t -> string = function
    | `Null         -> Postgresql.null
    | `Bool       x -> if x then "t" else "f"
    | `Int        x -> Int.to_string x
    | `Int64      x -> Int64.to_string x
    | `Float      x -> Float.to_string x
    | `String     x -> x
    | `Time       x -> Time.to_string x
    | `Date       x -> Date.to_string x
    | `Time_ofday x -> Time.Ofday.to_string x
    | `Time_span  x -> Time.Span.to_string x
    | `Uuid       x -> Uuidm.to_string x
    | `Json       x -> Yojson.Safe.to_string x
    | `Bytes      x -> Cstruct.to_string x
    | `Ipaddr     x -> Ipaddr.to_string x
    | `Ipaddr_prefix x -> Ipaddr.Prefix.to_string x
    | `Macaddr    x -> Macaddr.to_string x
    | `Otherwise (_, v) -> v

  let sexp_of_t (t:t) =
    let sexp tag v = Sexp.List [ Atom tag ; Atom v ] in
    match t with
    | `Null         -> Sexp.Atom "Null"
    | `Bool       x -> sexp "Bool"      Bool.(to_string x)
    | `Int        _ -> sexp "Int"       (to_internal t)
    | `Int64      _ -> sexp "Int64"     (to_internal t)
    | `Float      _ -> sexp "Float"     (to_internal t)
    | `String     _ -> sexp "String"    (to_internal t)
    | `Time       _ -> sexp "Time"      (to_internal t)
    | `Date       _ -> sexp "Date"      (to_internal t)
    | `Time_ofday _ -> sexp "Time_ofday" (to_internal t)
    | `Time_span  _ -> sexp "Time_span" (to_internal t)
    | `Uuid       _ -> sexp "Uuid"      (to_internal t)
    | `Json       _ -> sexp "Json"      (to_internal t)
    | `Bytes      _ -> sexp "Bytes"     (to_internal t)
    | `Ipaddr     _ -> sexp "Ipaddr"    (to_internal t)
    | `Ipaddr_prefix _ -> sexp "Ipaddr_prefix" (to_internal t)
    | `Macaddr    _ -> sexp "Macaddr"   (to_internal t)
    | `Otherwise (oid, v) ->
      Sexp.List [ Atom "Otherwise" ; Atom Int.(to_string oid) ; Atom v ]

  let get_value_f (result: Postgresql.result) : Postgresql.result -> int -> int -> t =
    let f ifield =
      let bool_of_string = function
        | "f" -> false
        | "t" -> true
        | _ -> raise (Invalid_argument "")
      in
      let bytes_of_string s =
        Cstruct.of_string Postgresql.(unescape_bytea s)
      in
      match result#ftype_oid ifield with
      |   16 -> fun value -> `Bool (bool_of_string value)
      |   20 -> fun value -> `Int64 Int64.(of_string value)
      |   21 -> fun value -> `Int Int.(of_string value)
      |   23 -> fun value -> `Int Int.(of_string value)
      |   25 -> fun value -> `String value
      |  700 -> fun value -> `Float Float.(of_string value)
      |  701 -> fun value -> `Float Float.(of_string value)
      | 1184 -> fun value -> `Time Time.(of_string value)
      | 1114 -> fun value -> `Time Time.(of_string value)
      | 1082 -> fun value -> `Date Date.(of_string value)
      | 1083 -> fun value -> `Time_ofday Time.Ofday.(of_string value)
(*    | 1266 -> fun value -> `Time_ofday (time_ofday_of_string value)
      | 1186 -> fun value -> `Time_span Time.Span.(of_string value)*)
      | 2950 -> fun value -> `Uuid Uuidm.(of_string value |> Option.value_exn)
      | 1042 -> fun value -> `String value
      | 1043 -> fun value -> `String value
      |  114 -> fun value -> `Json Yojson.Safe.(from_string value)
      | 3802 -> fun value -> `Json Yojson.Safe.(from_string value)
      |   17 -> fun value -> `Bytes (bytes_of_string value)
      |  869 -> fun value -> `Ipaddr Ipaddr.(of_string_exn value)
      |  650 -> fun value -> `Ipaddr_prefix Ipaddr.Prefix.(of_string_exn value)
      |  829 -> fun value -> `Macaddr Macaddr.(of_string_exn value)
      | oid  -> fun value -> `Otherwise (oid, value)
    in
    let conv = Array.init result#nfields ~f in
    fun (result: Postgresql.result) ituple ifield ->
      if result#getisnull ituple ifield then
        `Null
      else
        let value = result#getvalue ituple ifield in
        conv.(ifield) value
end

let exec ~(db: Postgresql.connection) ?expect ?(params=[||]) query : Value.t array list =
  let params = Array.map params ~f:Value.to_internal in
  let re = db#exec ?expect ~params query in
  let ntuples = re#ntuples in
  let nfields = re#nfields in
  let get_value_f = Value.get_value_f re in
  let acc = ref [] in
  for i = ntuples - 1 downto 0 do
    acc := Array.(init nfields ~f:(get_value_f re i)) :: !acc
  done;
  !acc

let with_transaction ~(db: Postgresql.connection) ~f =
  let begin_transaction () =
    db#exec {|
      BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE;
      SAVEPOINT Pgez_with_transaction_sp1;
    |} ~expect:[ Command_ok ]
    |> ignore
  in
  let rollback_transaction () =
    db#exec {| ROLLBACK TRANSACTION |} ~expect:[ Command_ok ]
    |> ignore
  in
  let rec loop () =
    match f () with
    | Error _ as ans ->
      rollback_transaction ();
      ans

    | Ok _ as ans ->
      let open Postgresql in
      match (db#exec {| COMMIT TRANSACTION |} ~expect:[ Command_ok ])
            #error_code with
      | Error_code.SERIALIZATION_FAILURE
      | Error_code.DEADLOCK_DETECTED ->
        db#exec {|
          ROLLBACK TRANSACTION TO SAVEPOINT Pgez_with_transaction_sp1;
        |} ~expect:[ Command_ok ]
        |> ignore;

        loop ()

      | _ -> ans
  in
  try
    begin_transaction ();
    loop ()
  with
  | exn ->
    Error (`Pgez_transaction_abort exn)
