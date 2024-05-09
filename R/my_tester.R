voltage = seq(-2, 1, 0.01)
current = seq(5, 50, length.out = length(voltage))

If = extract_i_from_iv(voltage, current, V0 = -1.07)




# Pf = extract_pf_from_pi(example_liv$`current[mA]`, example_liv$`power[mW]`, 20)

I = seq(0, 10, by = 0.1)
P = sqrt(seq(0, 5, length.out = length(I)))
P = -(I-5.123)^2 + 27.3453
Isat = extract_isat_from_pi(I, P)
Iop = extract_iop_from_pi(I, P, Pop = 25.4)
