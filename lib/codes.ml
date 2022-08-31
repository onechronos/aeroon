(** Client status codes *)
module Client = struct
  type t = int

  let st_AERON_CLIENT_ERROR_DRIVER_TIMEOUT = -1000
  let st_AERON_CLIENT_ERROR_CLIENT_TIMEOUT = -1001
  let st_AERON_CLIENT_ERROR_CONDUCTOR_SERVICE_TIMEOUT = -1002
  let st_AERON_CLIENT_ERROR_BUFFER_FULL = -1003

  let to_string (self : t) : string =
    if self = st_AERON_CLIENT_ERROR_DRIVER_TIMEOUT then
      "status: driver timeout"
    else if self = st_AERON_CLIENT_ERROR_CLIENT_TIMEOUT then
      "status: client timeout"
    else if self = st_AERON_CLIENT_ERROR_CONDUCTOR_SERVICE_TIMEOUT then
      "status: conductor service timeout"
    else if self = st_AERON_CLIENT_ERROR_BUFFER_FULL then
      "status: buffer full"
    else
      "status: unknown"
end

(** Publication status codes *)
module Publication = struct
  type t = int64

  (**
   * The publication is not connected to a subscriber, this can be an intermittent state as subscribers come and go.
   *)
  let st_NOT_CONNECTED = -1L

  (**
   * The offer failed due to back pressure from the subscribers preventing further transmission.
   *)
  let st_BACK_PRESSURED = -2L

  (**
   * The offer failed due to an administration action and should be retried.
   * The action is an operation such as log rotation which is likely to have succeeded by the next retry attempt.
   *)
  let st_ADMIN_ACTION = -3L

  (**
   * The publication has been closed and should no longer be used.
   *)
  let st_CLOSED = -4L

  (**
   * The offer failed due to reaching the maximum position of the stream given term buffer length times the total
   * possible number of terms.
   * <p>
   * If this happens then the publication should be closed and a new one added. To make it less likely to happen then
   * increase the term buffer length.
   *)
  let st_MAX_POSITION_EXCEEDED = -5L

  (**
   * An error has occurred. Such as a bad argument.
   *)
  let st_ERROR = -6L

  let to_string (st : t) : string =
    if st = st_ADMIN_ACTION then
      "status: admin action"
    else if st = st_BACK_PRESSURED then
      "status: back pressured"
    else if st = st_MAX_POSITION_EXCEEDED then
      "status: max position exceeded"
    else if st = st_NOT_CONNECTED then
      "status: not connected"
    else if st = st_CLOSED then
      "status: publication closed"
    else if st = st_ERROR then
      "status: publication error"
    else
      "status: unknown"
end
