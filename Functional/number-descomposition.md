## Particiones (Usando Python)

Este código genera todas las formas en que un número entero $n$ puede ser escrito como una suma de enteros positivos, donde el orden no importa (particiones de un entero).

---

### **Función Principal**

```python
def descomponer_en_sumas(n):
    """
    Genera todas las particiones de un número entero positivo n.
    """
    if n <= 0:
        return []

    def particiones_recursivas(target, max_val, actual_particion, resultados):
        """
        Función auxiliar recursiva que construye las particiones.
        """
        if target == 0:
            # Caso Base: La partición está completa
            resultados.append(actual_particion)
            return

        # Iteramos desde max_val (o target, si es menor) hasta 1.
        for i in range(min(max_val, target), 0, -1):
            # i es el siguiente sumando.
            
            # Llamada recursiva:
            # 1. target - i: Es el nuevo objetivo restante.
            # 2. i: Es el nuevo sumando máximo permitido (para mantener el orden y evitar duplicados).
            # 3. actual_particion + [i]: Se añade el sumando a la partición.
            particiones_recursivas(
                target - i, 
                i, 
                actual_particion + [i], 
                resultados
            )
            
    todas_las_sumas = []
    # La llamada inicial usa n como objetivo y como valor máximo permitido.
    particiones_recursivas(n, n, [], todas_las_sumas)
    
    return todas_las_sumas
```
## Explicación del Funcionamiento
El truco de este código está en usar la **recursión** (la función se llama a sí misma) con una ayuda extra, la función `particiones_recursivas`. Para evitar que nos salgan combinaciones repetidas (como contar `3 + 1` y luego `1 + 3` como sumas diferentes), usamos un parámetro clave: **`max_val`**. Este parámetro es como un **límite máximo** para el siguiente número que podemos sumar. Cada vez que elegimos un número nuevo, nos aseguramos de que sea **menor o igual** al número que elegimos justo antes. Esto obliga a que la lista de números vaya siempre **de mayor a menor**, eliminando los duplicados. El proceso sigue así, reduciendo el **objetivo** (`target`) hasta que llegamos a **cero**. Cuando el objetivo llega a cero, ¡listo! Hemos encontrado una suma completa, la guardamos y regresamos.

