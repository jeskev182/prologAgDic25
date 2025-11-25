## ***Preorden (Raíz-Izquierda-Derecha)***

```c
**void recorrerPreorden(Nodo *ptr) {
    if (ptr != NULL) {
        printf("%d ", ptr->dato);  // Visitar nodo
        recorrerPreorden(ptr->izquierda);  // Recorrer subárbol izquierdo
        recorrerPreorden(ptr->derecha);  // Recorrer subárbol derecho
    }
}**

```

## ***Inorden (Izquierda-Raíz-Derecha)***

```c
**void recorrerInorden(Nodo *ptr) {
    if (ptr != NULL) {
        recorrerInorden(ptr->izquierda);  // Recorrer subárbol izquierdo
        printf("%d ", ptr->dato);  // Visitar nodo
        recorrerInorden(ptr->derecha);  // Recorrer subárbol derecho
    }
}**
```

## ***Postorden (Izquierda-Derecha-Raíz)***

```c
**void recorrerPostorden(Nodo *ptr) {
    if (ptr != NULL) {
        recorrerPostorden(ptr->izquierda);  // Recorrer subárbol izquierdo
        recorrerPostorden(ptr->derecha);  // Recorrer subárbol derecho
        printf("%d ", ptr->dato);  // Visitar nodo
    }
}**
```

**La diferencia principal entre estos tres métodos es el orden en que se visita el nodo actual en relación con sus subárboles izquierdo y derecho.**