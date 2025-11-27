La Tierra es un planeta.

Variables:
Constantes individuales: t (Tierra)
Cuantificador:
Propiedades: Planeta
Relaciones:
Dominio del discurso: cuerpos celestes / objetos astronómicos



La Luna no es un planeta.

Variables:
Constantes individuales: l (Luna)
Cuantificador:
Propiedades: Planeta
Relaciones:
Dominio del discurso: cuerpos celestes



La Luna es un satélite.

Variables:
Constantes individuales: l (Luna)
Cuantificador:
Propiedades: Satelite
Relaciones:
Dominio del discurso: cuerpos celestes



La Tierra gira alrededor del Sol.

Variables:
Constantes individuales: t (Tierra), s (Sol)
Cuantificador:
Propiedades: Planeta
Relaciones: GiraAlrededor(x,y), GiraAlrededor(t,s)
Dominio del discurso: cuerpos celestes


Todo planeta es un satélite.

Variables: x
Constantes individuales:
Cuantificador: ∀x
Propiedades: Planeta(x), Satelite(x)
Relaciones:
Dominio del discurso: cuerpos celestes / objetos astronómicos


Todo planeta gira alrededor del Sol.

Variables: x
Constantes individuales: s (Sol)
Cuantificador: ∀x
Propiedades: Planeta(x)
Relaciones: GiraAlrededor(x,s)
Dominio del discurso: cuerpos celestes


Algún planeta gira alrededor de la Luna.

Variables: x
Constantes individuales: l (Luna)
Cuantificador: ∃x
Propiedades: Planeta(x)
Relaciones: GiraAlrededor(x,l)
Dominio del discurso: cuerpos celestes


Hay por lo menos un satélite.

Variables: x
Constantes individuales:
Cuantificador: ∃x
Propiedades: Satelite(x)
Relaciones:
Dominio del discurso: cuerpos celestes


Todos los perros del vecindario muerden a algún cartero.

Variables: x (perro), y (cartero)
Constantes individuales:
Cuantificador: ∀x sobre perros del vecindario; dentro existe ∃y (algún cartero por cada perro)
Propiedades: Perro(x), Cartero(y)
Relaciones: Muerde(x,y)
Dominio del discurso: perros y carteros del vecindario


Hay un cartero al que lo muerden todos los perros

Variables: x (perro), y (cartero)
Constantes individuales:
Cuantificador: ∃y (existe un cartero tal que) ∀x (todos los perros lo muerden)
Propiedades: Perro(x), Cartero(y)
Relaciones: Muerde(x,y)
Dominio del discurso: perros y carteros del vecindario


Todos los carteros son mordidos por algún perro

Variables: x (perro), y (cartero)
Constantes individuales:
Cuantificador: ∀y (para cada cartero existe ∃x perro)
Propiedades: Perro(x), Cartero(y)
Relaciones: Muerde(x,y)
Dominio del discurso: perros y carteros del vecindario


Hay un perro que muerde a todos los carteros

Variables: x (perro), y (cartero)
Constantes individuales:
Cuantificador: ∃x (existe un perro) tal que ∀y (para todo cartero)
Propiedades: Perro(x), Cartero(y)
Relaciones: Muerde(x,y)
Dominio del discurso: perros y carteros del vecindario


Todos los perros que asustan a algún cartero, lo muerden

Variables: x (perro), y (cartero)
Constantes individuales:
Cuantificador: ∀x (consideramos cada perro); condicional con ∃y (algún cartero al que asusta)
Propiedades: Perro(x), Cartero(y)
Relaciones: Asusta(x,y), Muerde(x,y)
Dominio del discurso: perros y carteros del vecindario


Hay un perro que muerde a todos los perros que muerden a algún cartero

Variables: x (perro), y (cartero), z (perro que muerde a otro)
Constantes individuales:
Cuantificador: ∃x (existe un perro x) tal que ∀z (para todo perro z que cumple la condición “∃y cartero con Muerde(z,y)”), se tiene Muerde(x,z)
Propiedades: Perro(x), Perro(z), Cartero(y)
Relaciones: Muerde(z,y), Muerde(x,z)
Dominio del discurso: perros y carteros del vecindario


Hay un solo perro que se muerde a sí mismo

Variables: x
Constantes individuales:
Cuantificador: ∃!x
Propiedades: Perro(x)
Relaciones: Muerde(x,x)
Dominio del discurso: perros