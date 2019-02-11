# Pgez

Eazy wrapper of postgresql.

```
let db = new Postgresql.connection () in

Pgez.with_transaction ~db ~f:begin fun () ->
  Pgez.exec ~db {|
    SELECT name FROM Names
    WHERE uid = $1
  |} ~params:[| `Int 1234 |]
  |> Result.return
end
|> function
| Ok [ [| `String name |] ] ->
  Log.Global.info "found %s." name
| Ok r -> r
          |> [%sexp_of: Pgez.Value.t array list]
          |> Sexp.to_string
          |> Log.Global.info "%s"
| Error _ ->
  ();
```
