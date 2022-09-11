let to_micro x = x /. 1000.

let stats list =
  let a = Array.of_list list in
  Array.sort Stdlib.compare a;

  let n = Array.length a in
  let nf = float n in

  let sum, sum_sq =
    Array.fold_left
      (fun (sum, sum_sq) x ->
        assert (x >= 0);
        let x = float x in
        sum +. x, sum_sq +. (x *. x))
      (0., 0.) a
  in
  let mean = sum /. nf in
  let variance = (sum_sq /. nf) -. (mean *. mean) in
  let std_dev = sqrt variance in
  let n_tile k = truncate (k *. nf) in
  let x_500 = a.(n_tile 0.500) in
  let x_990 = a.(n_tile 0.990) in
  let x_999 = a.(n_tile 0.999) in
  Printf.printf
    "n=%d\nmean=%0.2f±%0.2f\n50%%-ile=%0.2f\n99%%-ile=%0.2f\n99.9%%-ile=%0.2f\n"
    n (to_micro mean) (to_micro std_dev)
    (to_micro (float x_500))
    (to_micro (float x_990))
    (to_micro (float x_999))
