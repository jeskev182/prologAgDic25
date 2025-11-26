# Solución Concisa en Lisp (car y cdr)

## 1. Caso: `(a b c (d 1) (c x) m n) ->d`

| Ejercicio | Procedimiento | Resultado |
| :--- | :--- | :--- |
| `(a b c (d 1) (c x) m n)` | `(caadddr '(a b c (d 1) (c x) m n))` | `D` |

---

## 2. Caso: `(a b c (d) e f ((g h)) i j k) ->b`

| Ejercicio | Procedimiento | Resultado |
| :--- | :--- | :--- |
| `(a b c (d) e f ((g h)) i j k)` | `(cadr '(a b c (d) e f ((g h)) i j k))` | `B` |

---

## 3. Caso: `(((a b c d)) 1 (2) 3 (4 5) ((6 (7) 8))) ->4`


| Ejercicio | Procedimiento | Resultado |
| :--- | :--- | :--- |
| `(((a b c d)) 1 (2) 3 (4 5) ((6 (7) 8)))` | `(car (cdddr (cddr '(((a b c d)) 1 (2) 3 (4 5) ((6 (7) 8))))))` | `4` |
| | **Alternativa más simple:** `(car (nth 4 '(((a b c d)) 1 (2) 3 (4 5) ((6 (7) 8)))))` | `4` |