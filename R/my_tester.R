

wav = seq(1300, 1320, by = 0.1)
pow = rep(-70, length(wav)) + rnorm(length(wav), 0, 1)
pow[101:103] = c(-30, -5, -30)
pow[50:52] = c(-40, -38, -40)
bandwidth = extract_bw20dB(wav, pow)
print(bandwidth)
