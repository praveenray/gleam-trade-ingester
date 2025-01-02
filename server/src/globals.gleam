import trades/writer_actor_messages as writer_messages
import gleam/erlang/process.{type Subject}
import gleam/erlang.{type Reference}
import gleam/option.{type Option}
import gleam/erlang/atom.{type Atom}

pub const writer_subject = "writer_subject"

@external(erlang, "globals_erlang", "ets_table_from_file")
pub fn ets_table_from_file(file_name: String, table_name: String) -> Result(Reference, String)

@external(erlang, "globals_erlang", "create_ets_table")
pub fn create_ets_table(table_name: String) -> Result(Reference, a)

pub fn add_trade_loader_subject(subject: Subject(writer_messages.Message), tid: Reference) -> Bool {
  insert(tid, writer_subject, subject)
}
pub fn get_trade_loader_subject(tid: Reference) -> Option(Subject(writer_messages.Message)) {
  case lookup_key(tid, writer_subject) {
    [#(_, subject)] -> option.Some(subject)
    _ -> option.None
  }
}


@external(erlang, "globals_erlang", "delete_ets_key")
pub fn delete_ets_key(table: Reference, key: String) -> Bool

@external(erlang, "globals_erlang", "lookup_atom_key")
fn lookup_key(table: Reference, key: String) -> List(#(Atom, Subject(a)))

@external(erlang, "globals_erlang", "insert")
fn insert(table: Reference, key: String, subject: Subject(a)) -> Bool
