open! Core

module Query_args : sig
  type t =
    { output_mode : Query.output_mode
    ; allow_empty_output : bool
    ; labeled : bool option
    }
end

val query_args : Query_args.t Command.Param.t

module Machine_and_fail_on_parse_error : sig
  type t =
    { machine : bool
    ; fail_on_parse_error : bool
    }
end

val machine_and_fail_on_parse_error : Machine_and_fail_on_parse_error.t Command.Param.t
val machine : bool Command.Param.t
val channel_stdin_or_anon_file : In_channel.t Command.Param.t
