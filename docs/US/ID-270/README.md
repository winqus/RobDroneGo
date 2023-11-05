# ID 270 - Create an elevator in a building

## Description
As user, I want to do a post request to the api to create a elevator on one building

## Acceptance Criteria

* POST method can be called to create an elevator.
* Appropriate HTTP Status codes are returned (201 / 202 / 400).
* Unit tests are written and passed.

## Questions from the forum

> 
> Question (14/10/2023)
> > "Tendo em conta que só há um elevador por edificio, pretende-se saber qual é o elevador de cada edificio? Ou é suposto haver mais do que um elevador num edificio?" <br>
>
> Answer (16/10/2023)
> > "bom dia, de momento apenas necessitam suportar um elevador por edificio" <br>

> Question (19/10/2023)
> > "Como tal, gostaria de saber que atributos deveria ter o elevador, para além de uma lista de pisos aos quais consegue aceder dentro do seu edifício. Algumas das ideias que me surgiram foram o piso em que estava localizado naquele momento, número de série, fabricante ou descrição." <br>
>
> Answer (20/10/2023)
> > "bom dia, <br>
edificio (obrigatório) <br>
número identificativo (obrigatório, único no edificio) <br>
lista de pisos do edificio servidos pelo elevador (obrigatório) <br>
marca (opcional, alfanumerico, 50 caracteres)<br>
modelo (opcional, mas obrigatório se marca for introduzido, alfanumerico, 50 caracteres)<br>
número de série do fabricante (opcional,alfanumerico, 50 caracteres)<br>
breve descrição (opcional, alfanumerico, 250 caracteres)" <br>

> Question (23/10/2023)
> > "Gostaríamos que clarificasse quais das propriedades que indicou serem alfanuméricas podem conter espaços; por exemplo, nós acharíamos que seria sensato a descrição poder conter espaços. <br>
Adicionalmente, gostaria de saber se o identificador numérico que referiu deve ser fornecido ao sistema ou gerado automaticamente pelo mesmo, dado que este deve ser único dentro de cada edifício." <br>
>
> Answer (23/10/2023)
> > "bom dia,
todos os atributos alfanumercos podme conter espaços à exceção do número de série <br>
o número indeitifcativo do elevador deve ser gerado sequencialmente pelo sistema tendo em conta o edifico, por exemplo, existirá o elevador 1 do edificio B e o elevador 1 do edificio A" <br>


## Diagramns

### Logical View Lv1
![LV Lv1](../../diagrams/level1/Logical%20View%20Lv1.svg)

### Logical View Lv2
![LV Lv2](../../diagrams/level2/Logical%20View%20Lv2.svg)

### Logical View Lv3
![LV Lv3](../../diagrams/level3/Logical%20View%20lv3%20(Campus%20Management).svg)

### Deployment View
![DV Lv1](../../diagrams/Deployment%20View.svg)

### Process Diagram Lv1
![SD Lv1](./SD%20Lv1.svg)

### Process Diagram Lv2
![SD Lv2](./SD%20Lv2.svg)

### Process Diagram Lv3
![SD Lv3](./SD%20Lv3.svg)

### Domain Model
![DM](../../diagrams/DM.png)

## Observations