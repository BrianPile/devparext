
i = example_liv$`current[mA]`
p = example_liv$`power[mW]`
v = example_liv$`voltage[V]`

purrr::map_dbl(c(1, 1.1), \(dbl) extract_vf_from_vi(i, v, dbl))


