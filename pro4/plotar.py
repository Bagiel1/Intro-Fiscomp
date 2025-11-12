import matplotlib.pyplot as plt

arquivo = "chute_out.dat"

x, y = [], []

with open(arquivo, 'r') as f:
    for linha in f:
        linha = linha.strip()
        if not linha or linha.lower().startswith('x'): 
            continue
        partes = linha.replace(',', ' ').split()
        if len(partes) >= 2:
            x.append(float(partes[0]))
            y.append(float(partes[1]))

plt.plot(x, y, 'bo-', linewidth=2)
plt.title("Gráfico de X vs Y")
plt.xlabel("X")
plt.ylabel("Y")
plt.grid(True)
plt.show()
