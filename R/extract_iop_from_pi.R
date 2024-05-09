extract_iop_from_pi = function(I, P, Pop = 10e-3) {

  # if the P-I curve doesn't reach Pop, return NA
  if (max(P) <= Pop) {
    return(NA)
  }

  # get Isat and then get only powers where current is less than Isat
  Isat = extract_isat_from_pi(I, P)
  P = P[I < Isat]

  idx = which.min(abs(P - Pop))
  Iop = I[idx]
}
