## 📝 Evaluación de Expresiones de Paréntesis

La lógica para determinar si una secuencia de paréntesis es válida (bien balanceada) se realiza utilizando una **pila (stack)**.

1.  **Encontrar `(`**: Se **empuja** (push) a la pila.
2.  **Encontrar `)`**: Se **saca** (pop) un elemento de la pila.
3.  **Error Inmediato**: Si se intenta sacar un elemento de una pila **vacía**, la secuencia es **inválida**.
4.  **Verificación Final**: Al terminar de recorrer la secuencia, la pila debe quedar **vacía** para ser válida.

---

### 1. Evaluación: `( ( ) ) ) ( ( )`

| Carácter | Acción | Pila (Stack) | Resultado |
| :---: | :---: | :---: | :--- |
| `(` | Push | `(` | |
| `(` | Push | `( (` | |
| `)` | Pop | `(` | |
| `)` | Pop | Vacio | |
| **`)`** | **Pop** | **Error** | **INVÁLIDA: Cierre sin apertura** |
| `(` | (No se evalúa) | | |
| `(` | (No se evalúa) | | |
| `)` | (No se evalúa) | | |

**Conclusión:** La expresión es **INVÁLIDA**. El tercer paréntesis de cierre (`)`) intenta hacer `pop` en una pila vacía.

---

### 2. Evaluación: `( ( ) ) ( ) ( ) )`

| Carácter | Acción | Pila (Stack) | Resultado |
| :---: | :---: | :---: | :--- |
| `(` | Push | `(` | |
| `(` | Push | `( (` | |
| `)` | Pop | `(` | |
| `)` | Pop | Vacio | |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| **`)`** | **Pop** | **Error** | **INVÁLIDA: Cierre sin apertura** |

**Conclusión:** La expresión es **INVÁLIDA**. El último paréntesis de cierre (`)`) intenta hacer `pop` en una pila vacía.

---

### 3. Evaluación: `( ) ( ) ) ( ( ) )`

| Carácter | Acción | Pila (Stack) | Resultado |
| :---: | :---: | :---: | :--- |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| **`)`** | **Pop** | **Error** | **INVÁLIDA: Cierre sin apertura** |
| `(` | (No se evalúa) | | |
| `(` | (No se evalúa) | | |
| `)` | (No se evalúa) | | |
| `)` | (No se evalúa) | | |

**Conclusión:** La expresión es **INVÁLIDA**. El tercer paréntesis de cierre (`)`) intenta hacer `pop` en una pila vacía.

---

### 4. Evaluación: `( ( ( ) ) ) ( ) ( )`

| Carácter | Acción | Pila (Stack) | Resultado |
| :---: | :---: | :---: | :--- |
| `(` | Push | `(` | |
| `(` | Push | `( (` | |
| `(` | Push | `( ( (` | |
| `)` | Pop | `( (` | |
| `)` | Pop | `(` | |
| `)` | Pop | Vacio | |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| `(` | Push | `(` | |
| `)` | Pop | Vacio | |
| **Fin** | **Verificación** | **Vacio** | **VÁLIDA** |

**Conclusión:** La expresión es **VÁLIDA**. La pila queda vacía al final del recorrido.