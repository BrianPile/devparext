
x = -20:20
n = rnorm(length(x), mean = 0, sd = 5)
y = 5*x + 3 + n

df = my_linear_fit(x, y)

plot(x, y, type = "p")

abline(a = df$b, b = df$m)

print(df)
