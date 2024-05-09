voltage = seq(-2, 1, 0.01)
current = seq(5, 50, length.out = length(voltage))

If = extract_i_from_iv(voltage, current, V0 = -1.07)




# Pf = extract_pf_from_pi(example_liv$`current[mA]`, example_liv$`power[mW]`, 20)

Pf = extract_pf_from_pi(1:10, 1:10, 1)
