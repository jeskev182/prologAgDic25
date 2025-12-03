series en Prolog ---

Las explicaciones se centran en el concepto matemático principal de cada serie

## 1.
Números: 8, 3, -2, -7, -12
serie1(N, X) :- X is 8 - 5*(N-1).

Explicación: Esta es una **Progresión Aritmética**. Lo que pasa aquí es que a cada término se le resta una cantidad fija, que es la **diferencia común** de -5. Empieza en 8 y el -5 se va aplicando progresivamente (N-1) veces.

---

## 2.
Números: 3, 6, 12, 24, 48
serie2(N, X) :- X is 3 * 2**(N-1).

Explicación: Esta es una **Progresión Geométrica**. El truco es que cada número es el doble del anterior. La **razón** es 2. La fórmula es el valor inicial (3) multiplicado por esta razón (2) elevada a la potencia de la posición (N-1).

---

## 3.
Números: 4, 9, 16, 25, 36, 49
serie3(N, X) :- X is (N+1)**2.

Explicación: La serie está hecha de **cuadrados perfectos**, como $2^2, 3^2, 4^2$, etc. Como el primer término es 4 (que es $2^2$), la fórmula tiene que estar desplazada. Por eso se usa $(N+1)^2$ para asegurar que cuando $N=1$, el resultado sea $2^2$.

---

## 4.
Números: 5, 10, 17, 26, 37, 50
serie4(N, X) :- X is N*N + 2*N + 2.

Explicación: Esta es una **Progresión Cuadrática**, lo que significa que el crecimiento es curvo y acelerado. La fórmula es un **polinomio de segundo grado** ($N^2 + 2N + 2$). El término $N^2$ es lo que define esta aceleración en el aumento.

---

## 5.
Números: 6, 11, 18, 27, 38, 51
serie5(N, X) :- X is N*N + 2*N + 3.

Explicación: Es la misma **Progresión Cuadrática** que la anterior, con la misma estructura $N^2 + 2N$. La única diferencia es que el **término constante es $+3$**. Por eso, todos los números de esta serie son exactamente 1 más grandes que los de la Serie 4.

---

## 6.
Números: 3, 8, 15, 24, 35, 48
serie6(N, X) :- X is N*N + 2*N.

Explicación: También es una fórmula **cuadrática**, pero sin término constante (es decir, el constante es 0). Se puede ver como la multiplicación de la posición ($N$) por el valor de la posición más dos ($N+2$). Su fórmula es $N(N+2)$.

---

## 7.
Números: -4, 9, -16, 25, -36, 49
serie7(N, X) :- Signo is (-1)**N, X is Signo * (N+1)**2.

Explicación: Los valores absolutos son los **cuadrados desplazados** (como en la Serie 3). Lo interesante es el **signo alternante** $(-1)^N$. Como $N$ es la potencia, cuando $N=1$ el signo es negativo, así que la serie empieza en negativo.

---

## 8.
Números: 4, -9, 16, -25, 36, -49
serie8(N, X) :- Signo is (-1)**(N-1), X is Signo * (N+1)**2.

Explicación: Es casi igual que la anterior, los valores absolutos son cuadrados. La diferencia es el factor de signo: $(-1)^{(N-1)}$. Esto hace que el primer término ($N=1$) sea positivo (porque el exponente es 0), y la alternancia empieza con el segundo término.

---

## 9.
Números: 2/4, 5/9, 8/16, 11/25, 14/36
serie9(N, X) :- Num is 3*N - 1, Den is (N+1)**2, X is Num / Den.

Explicación: Aquí hay dos patrones en uno, una **Serie Racional**. El **numerador** es una **progresión aritmética** que va sumando 3 en cada paso ($3N - 1$). El **denominador** son **cuadrados perfectos desplazados** ($(N+1)^2$).

---

## 10.
Números: −5, 7/2, −9/3


% --- Función Mapcar y Generación de Listas de Series en Prolog

Funcion Predicada: mapcar/3

El predicado mapcar(F, Nmax, Lista) está diseñado para construir una lista aplicando repetidamente una función específica (F) a una colección de números consecutivos.

Se logra esto mediante el uso de findall, que se encarga de recolectar todos los resultados en una única lista.

Flujo de la Función:
1. Iteración: Usa between(1, Nmax, N) para generar el índice N que va desde 1 hasta el número máximo de elementos (Nmax).
2. Ejecución: Por cada N generado, se llama a la función de la serie con call(F, N, X), lo que calcula el valor del término X en esa posición.
3. Recolección: findall junta todos esos valores X y los unifica con la variable Lista.

mapcar(F, Nmax, Lista) :- findall(X, (between(1, Nmax, N), call(F, N, X)), Lista).

---

Aplicación a las Series (serie*_lista/2)

Los predicados serie*_lista(N, Lista) son simplemente interfaces que invocan a mapcar/3.

Definen la función de la serie que debe usarse (por ejemplo, serie1, serie2) y la longitud de la lista a generar (N).

Donde N es el número de elementos y Lista es la variable que contendrá la serie generada.

Implementación de Interfaz:
serie1_lista(N, Lista) :- mapcar(serie1, N, Lista).
serie2_lista(N, Lista) :- mapcar(serie2, N, Lista).
serie3_lista(N, Lista) :- mapcar(serie3, N, Lista).
serie4_lista(N, Lista) :- mapcar(serie4, N, Lista).
serie5_lista(N, Lista) :- mapcar(serie5, N, Lista).
serie6_lista(N, Lista) :- mapcar(serie6, N, Lista).
serie7_lista(N, Lista) :- mapcar(serie7, N, Lista).
serie8_lista(N, Lista) :- mapcar(serie8, N, Lista).
serie9_lista(N, Lista) :- mapcar(serie9, N, Lista).
serie10_lista(N, Lista) :- mapcar(serie10, N, Lista).