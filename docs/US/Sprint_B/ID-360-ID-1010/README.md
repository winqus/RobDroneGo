# ID 360 - Add a new robot - ID 1010

## Description
As Fleet Manager, I want to add a new robot to a fleet indicating its type, desingation, etc.

## Acceptance Criteria
* Robots can be created and stored in DB.
* Unit tests are written and passed.

## Questions from the forum

> Question:
> 
>Ao criar um novo robo, qual o estado dele por defeito, isto é, ativo ou inativo?
Tendo em conta a US370 seria ativo por defeito certo?
> 
> >Answer:
> >
>>Ao criar um robot ele fica no estado ativo
> 
> Question:
> 
> Os atributos do robot têm algum tipo de formatação/restrição?
> 
> >Answer:
> >
> > Código identificativo, obrigatório, alfanumerico, max 30 caracteres, único no sistema
> >
>>nickname, obrigatório, obrigatório, alfanumerico, max 30 caracteres, único no sistema
> >
>>tipo de robot, obrigatório
> >
>>número de série, obrigatório, alfanumerico, max 50 caracteres, único para um dado tipo de robot
> >
>>descrição, opcional, alfanumerico, max. 250 caracteres
> 
> Question:
> 
> 
## Diagramns

![LVL1](../../../Sprint%20B%20diagrams/level_1/Logical%20View%20Lv1.svg)

### Logical View Lv2

![LVL2](../../../Sprint%20B%20diagrams/level_2/Implementation%20View%20Lv2.svg)

### Logical View Lv3

![LVL3](../../../Sprint%20B%20diagrams/level_3/Logical%20View%20lv3.svg)

### Deployment View

![DV](../../../Sprint%20B%20diagrams/Physical%20View.svg)


### Process Diagram Lv1

![PVL1](../../../out/US/Sprint_B/ID-360-ID-1010/SD%20lv1/SD%20Lv1.png)

### Process Diagram Lv2

![PVL2](../../../out/US/Sprint_B/ID-360-ID-1010/SD%20lv2/SD%20Lv2.png)

### Process Diagram Lv3

![PVL3](../../../out/US/Sprint_B/ID-360-ID-1010/SD%20lv3/SD%20Lv3.png)

### Domain Model

![DV](../../../diagrams/DomainDesign.png)
## Observations