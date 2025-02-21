# ID 350 - Create an Robot Type

## Description
As a fleet manager, I want to add a new type of robot.


## Acceptance Criteria

* It’s possible to create a new type of robots and choose its tasks from a pre-defined list of tasks.
* Unit tests are implemented and pass.

## Questions from the forum

> 
> Question (16/10/2023)
> > "Pretende alguma regra de negócio para o limite de caracteres para o tipo, marca e modelo?" <br>
>
> Answer (16/10/2023)
> > "bom dia,
tipo de robot: obrigatório, alfanuméricos, maximo 25 caracteres
marca: obrigatório, maximo 50 caracteres
modelo: obrigatório, máximo 100 caracteres" <br>

> Question (25/10/2023)
> > "Relativamente as restrições indicadas para o campo "tipo de robot", este campo indica o tipo de tarefa executada por um robot (pickup&delivery e/ou vigilancia) ou um atributo do tipo nome do tipo do robot, e existe outro campo para o tipo de tarefa executado por esse tipo de robot?" <br>
>
> Answer (25/10/2023)
> > "bom dia,
o "tipo de robot" é um código identificativo desse tipo de robots. a lista de capacidades do robot é uma informação diferente"

> Question (23/10/2023)
> > "Em relação aos atributos do tipo de robot existe alguma condição no modelo do tipo de  robot, por exemplo: podemos ter  um robot da marca A e modelo A1 e outro marca B e modelo A1?"
>
> Answer (23/10/2023)
> > "boa tarde,
marca e modelo são dois atributos de texto livre. o sistema não impoe restrições"

> Question (23/10/2023)
> > "No caderno de encargos, é referido que os drones movimentam-se no "espaço exterior aos edifícios existentes no ISEP" e que os robisep se movimentam através de um "sistema de rodas podendo deslocar-se nos corredores dos edifícios ou através de elevadores entre pisos de um edifício".
No entanto refere que "para esta fase do protótipo assume-se que os robots podem executar apenas as seguintes tarefas: transporte de um objeto de um ponto para outro e vigilância de segurança de um piso de um edifício efetuando recolha de imagens". Neste caso se quisermos criar um drone este não pode ter a tarefa de vigia uma vez que esta é relativa a um piso de um edifício(uma vez que este só pode vigiar o exterior)?"
>
> Answer (23/10/2023)
> > "bom dia,
os drones estão fora do âmbito do prototipo" 

> Question (23/10/2023)
> > "Existem diferentes tipos de tarefas, por isso, cada uma terá os seus atributos. No entanto, que atributos definem uma tarefa em geral? Apenas a sua designação? <br>
Em relação às tarefas existentes (vigilância de um piso e transporte de um objeto) existem algum requerimento especial? Para além da especificação do piso na vigilância e especificação do objeto e local de recolha e entrega no caso do transporte."
>
> Answer (23/10/2023)
> > "Boa tarde,
de momento todos os robots da frota apenas suportam estes dois tipos de tarefas. a existirem novos tipos de tarefas será necessáiro desenvolvimento especifico para tal. Em consequência não existem "tarefas em geral" <br>
As tarefas de vigilância caracterizam-se por indicar qual o edificio e piso(s) que se pretende vigiar bem como o número de contacto em caso de incidente. tipicamente o segurança irá requisitar que um robot "dê uma volta pelos pisos X, Y e Z do edificio N". Caso o robot detete alguma situação anómala, o robot irá enviar um SMS para o contacto indicado (\*) <br>
As tarefas de "piclup & delivery" caracterizam-se por indicar qual a sala que se pretende de pickup e qual a sala de delivery, bem como um nome e um contacto para pickup e outro para delivery. deve também ser indicado um código de confirmação que a pessoa que receberá deverá introduzir no painel do robot. adicionalmente deve ser indicada uma descrição da entrega, ex., "Marcadores de cor vermelha (1x) e azul (2x)"<br>
(\*) fora do âmbito do protótipo" 


## Diagramns

### Logical View Lv1
![LV Lv1](../../diagrams/level1/Logical%20View%20Lv1.svg)

### Logical View Lv2
![LV Lv2](../../diagrams/level2/Logical%20View%20Lv2.svg)

### Logical View Lv3
![LV Lv3](../../diagrams/level3/Logical%20View%20Lv3%20(Fleet%20Management).svg)

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