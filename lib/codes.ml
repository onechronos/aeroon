module ABT = Aeroon_bindings.Types_generated

(** Client status codes *)
module Client = struct
  type t = int

  let error_driver_timeout = ABT.client_error_driver_timeout
  let error_client_timeout = ABT.client_error_client_timeout

  let error_conductor_service_timeout =
    ABT.client_error_conductor_service_timeout

  let error_buffer_full = ABT.client_error_buffer_full

  let code_msg_assoc =
    [
      error_driver_timeout, "status: driver timeout";
      error_client_timeout, "status: client timeout";
      error_conductor_service_timeout, "status: conductor service timeout";
      error_buffer_full, "status: buffer full";
    ]

  let to_string code =
    match List.assoc_opt code code_msg_assoc with
    | Some msg -> msg
    | None -> Printf.sprintf "status: unknown (%d)" code
end

(** Publication status codes *)
module Publication = struct
  type t = int64

  let not_connected = ABT.publication_not_connected
  let back_pressured = ABT.publication_back_pressured
  let admin_action = ABT.publication_admin_action
  let closed = ABT.publication_closed
  let max_position_exceeded = ABT.publication_max_position_exceeded
  let error = ABT.publication_error

  let code_msg_assoc =
    [
      not_connected, "status: not connected";
      back_pressured, "status: back pressured";
      admin_action, "status: admin action";
      closed, "status: closed";
      max_position_exceeded, "status: max position exceeded";
      error, "status: error";
    ]

  let to_string code =
    match List.assoc_opt code code_msg_assoc with
    | Some msg -> msg
    | None -> Printf.sprintf "status: unknown (%Ld)" code
end
