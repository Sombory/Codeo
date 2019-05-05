
# Importo la base housing para normalizar


# Hacemos con un normalización standar (z = xi - u / Oi). Esta normalización el centro es "0"
# y el maximo +1 y el mínimo -1

housing.z <-  scale(housing)

