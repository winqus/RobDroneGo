# ID 230 - Load floor map

## Description
As user, I want to load a floor map JSON file that would update existing floor object data.

## Acceptance Criteria

* PATCH method can be called and floor data is updated according to the file data.
* Appropriate HTTP Status codes are returned (200 / 204 / 400 / 404).
* Unit tests are written and passed.

## Questions from the forum

> 
> Question (9/10/2023)
> > "Será possível esclarecer como funcionarão estas user stories? Com a 230 (Carregar mapa do piso) o nosso entendimento foi que as células seriam carregadas já com a criação de salas e pisos, e assim sendo não faria sentido as outras duas user stories, onde é pedido para criar um piso de um edifício e uma sala. Não entendemos o que é pretendido  com as us's 190 e 310." <br>
>
> Answer (10/10/2023)
> > "bom dia,
o requisito 230 Carregar mapa de piso permite ao utlizador fazer upload de um ficheiro descrevendo o mapa de um dado piso. esse ficheiro deve ser validado se tem a estrutura correta e se obdece ao tamanho máximo definido aquando da criação do edificio" <br>

> 
> Question (9/10/2023)
> > "Segundo o que já foi respondido no forum para carregar o mapa é necessário que já exista o edifício e o piso. Será necessário também ter os elevadores, as salas e as passagens já criadas?.
Assim dessa forma existiria um ficheiro só com as dimensões para a grelha." <br>
>
> Answer (10/10/2023)
> > "bom dia,
sim, é necessário que essa informação já esteja presente no sistema. quanto ao formato do mapa, será fornecido um projeto exemplo em SGRAI para desenho de labirintos que podme utilizar como base para o módulo de visualização. poderão adaptar o código e o formato de mapa de acordo com o que acharem mais adequado aos requisitos e às vossas decisões de design." <br>

## Diagramns

### Logical View Lv1

### Logical View Lv2

### Logical View Lv3

### Deployment View

### Process Diagram Lv1

### Process Diagram Lv2

### Process Diagram Lv3

### Domain Model

## Observations