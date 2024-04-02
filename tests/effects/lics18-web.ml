(* Adapted from:
   A Fixpoint Logic and Dependent Effects for
   Temporal Property Verification
   Nanjo et al, LICS 2018 *)

let nondet i =
  (i * i * i * 199) mod 2

let rec listener i npool pend =
  if nondet i == 0 && pend < npool then begin
    ev 1; (* Accept *)
    listener i npool (pend + 1)
  end else if pend > 0 then begin
    ev 2; (* Handle *)
    listener i npool (pend - 1)
  end else begin
    ev 3; (* Wait *)
    listener i npool pend
  end

let server npool i0 =
  listener i0 npool 0
