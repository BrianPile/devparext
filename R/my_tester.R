

temps = c(25, 50, 75)
peak_wavs = c(1308, 1310.4, 1312.3)

extract_wl_temp_coef(temps, peak_wavs)

wav = seq(1300, 1320, by = 1)
pow = rep(-70, length(wav))
pow[10] = -10
peak_wav = extract_peak_wav(wav, pow)
