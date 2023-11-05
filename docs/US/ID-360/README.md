# ID 360 - Add a new robot

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

### Logical View Lv1

![LVL1](../../diagrams/level1/L1-LV.png)

### Logical View Lv2

![LVL2](../../diagrams/level2/L2-LV.png)

### Logical View Lv3

![LVL3](../../diagrams/level3/L3-LV.png)

### Deployment View

![DV](../../diagrams/Deployment View.png)

### Process Diagram Lv1

![PVL1](PV_lv1.png)

### Process Diagram Lv2

![PVL2](PV_lv2.png)

### Process Diagram Lv3

![PVL3](PV_lv3.png)

### Domain Model

![DV](../../diagrams/DM.png)

## Observations