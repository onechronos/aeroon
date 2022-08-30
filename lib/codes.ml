module Status = struct
  type t = int64

  let st_NOT_CONNECTED : t = -1L
  let st_BACK_PRESSURED : t = -2L
  let st_ADMIN_ACTION : t = -3L
  let st_PUBLICATION_CLOSED : t = -4L
  let st_MAX_POSITION_EXCEEDED : t = -5L
  let st_PUBLICATION_ERROR : t = -6L

  let to_string (st : t) : string =
    if st = st_ADMIN_ACTION then
      "status: admin action"
    else if st = st_BACK_PRESSURED then
      "status: back pressured"
    else if st = st_MAX_POSITION_EXCEEDED then
      "status: max position exceeded"
    else if st = st_NOT_CONNECTED then
      "status: not connected"
    else if st = st_PUBLICATION_CLOSED then
      "status: publication closed"
    else if st = st_PUBLICATION_ERROR then
      "status: publication error"
    else
      "status: unknown"
end
