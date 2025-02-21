# ID 370 - Deactivate a robot - ID 1020

## Description
As a fleet manager I want to deactivate a robot

## Acceptance Criteria
* Robots can be turned to non-active state.
* Unit tests written and passed.

## Questions from the forum

> Question:
> 
>Ao criar um novo robo, qual o estado dele por defeito, isto é, ativo ou inativo?
Tendo em conta a US370 seria ativo por defeito certo?
>
> > Answer:
> >
> >Ao criar um robo ele fica no estado ativo
> 
> Question:
> 
> Há a possibilidade de inibir um robo. No entanto, para além deste "estado" que outros 
> estados pretende que existam? em funcionamento, ocupado, livre, a executar tarefa? Ou 
> basta apenas inibido - desinibido?
> 
> > Answer:
> >
> > Funcionalmente não existe esse conceito de "estado" que referes. poderá ser no entanto 
> >algo util em termos técnicos. De um ponto de vista funcional pretende-se que seja possivel 
> >inibir ou desinibir um robot e que essa informação seja devidamente utilizada nos restantes 
> >casos de uso. por exemplo, um robot inibido não pode executar tarefas.
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

![PVL1](../../../out/US/Sprint_B/ID-370-ID-1020/SD%20lv1/SD%20Lv1.png)

### Process Diagram Lv2

![PVL2](../../../out/US/Sprint_B/ID-370-ID-1020/SD%20lv2/SD%20Lv2.png)

### Process Diagram Lv3

![PVL3](../../../out/US/Sprint_B/ID-370-ID-1020/SD%20lv3/SD%20Lv3.png)

### Domain Model

![DV](../../../diagrams/DomainDesign.png)

## Observations