## Contents
- [Architecture Background](#architecture-background)
    - [Problem Background](#problem-background)
        - [System Overview](#system-overview)
        - [Context](#context)
        - [Driving Requirements](#driving-requirements)
            - [Functional requirements](#functional-requirements)
            - [Quality attributes](#quality-attributes)
                - [Functionality](#functionality)
                - [Usability](#usability)
                - [Reliability (Confiabilidade)](#reliability-confiabilidade)
                - [Performance (Desempenho)](#performance-desempenho)
                - [Supportability](#supportability)
                - [Design constraints](#design-constraints)
                - [Implementation constraints](#implementation-constraints)
                - [Interface constraints](#interface-constraints)
                - [Physical constraints](#physical-constraints)
    - [Solution Background](#solution-background)
        - [Architectural Approaches](#architectural-approaches)
        - [Analysis Results](#analysis-results)
        - [Mapping Requirements to Architecture](#mapping-requirements-to-architecture)

# Architecture Background
> Architecture Background provides information about the software architecture, by:
> - describing the background and rationale for the software architecture;
> - explaining the constraints and influences that led to the current architecture;
> - describing the major architectural approaches that have been utilized in the architecture.
  
## Problem Background
> The sub-parts of this section explain the constraints that provided significant influence over the architecture.

### System Overview

> This section describes the general function and purpose of the system or subsystem whose architecture is described in this SAD.

The ISEP requires a comprehensive system designed to facilitate the management and navigation of campus facilities. This includes the ability to create, edit, and list buildings and their specific features such as floors, rooms, passages, and elevators. The system will serve as a platform for campus administrators to manage physical resources effectively, while also providing students and staff with the means to navigate the campus infrastructure efficiently.

### Context

> This section describes the goals and major contextual factors for the software architecture. The section includes a description of the role software architecture plays in the lifecycle, the relationship to system engineering results and artifacts, and any other relevant factors.

Campus management involves coordinating multiple facilities and logistical elements to support the institution's academic and operational goals. This includes optimizing space utilization, maintaining facilities, and ensuring safe and accessible movement throughout the campus. The system's architecture will thus need to facilitate a wide range of functionalities, from detailed administrative controls to user-friendly interfaces for information retrieval and navigation support.

*NB: This system is designed as a manageable project within the context of the 5th semester of Computer Science and Engineering (CSE) program. Certain complexities inherent in campus management systems may be abstracted to ensure the project's feasibility within this academic framework.*

For this semester's edition, we will engage with the expertise of companies specializing in architectural and facilities management software to provide practical insights into the development process. 

This SAD is not just a documentation artifact but also a strategic tool for guiding the development of the system. It will outline the architectural decisions, highlight design alternatives, and identify the system's points of variability.

The document targets a wide audience that includes students who, throughout their coursework, will take on various roles pertinent to the software development lifecycle. These roles include, but are not limited to, requirements gatherers, analysts, software architects, developers, testers, system administrators, and end-users.


### Driving Requirements
> This section lists the functional requirements, quality attributes, and design constraints. It may point to a separate requirements document.

#### Functional requirements
1. ID150 Create building
1. ID160 Edit building
1. ID170 List all buildings
1. ID180 List buildings with min and max floors
1. ID190 Create building floor
1. ID200 Edit building floor information
1. ID210 List all floors in a building
1. ID220 List building floors with passage to other buildings
1. ID230 Upload floor map
1. ID240 Create passage between buildings
1. ID250 Edit passage between buildings
1. ID260 List passages between 2 buildings
1. ID270 Create an elevator in a building
1. ID280 Edit elevator in building
1. ID290 List elevators in building
1. ID300 List building floors served by elevator
1. ID310 Create building floor room
1. ID350 As a fleet manager, I want to add a new type of robot indicating its designation and what types of tasks it can perform from the predefined list of tasks
1. ID360 As a fleet manager, I want to add a new robot to the fleet indicating its type, name, etc.
1. ID370 As a fleet manager, I want to inhibit a robot
1. ID380 As a fleet manager, I intend to consult all robots in the fleet
1. ID390 As a fleet manager, I want to search all the robots in the fleet by designation or task they can perform.
1. ID760 As a solution architect, I want a duly justified and clear diagram of which components will exist in the solution and what their integration interfaces are, indicating the type of information and information structure to be shared.


#### Quality attributes
Quality attributes are categorized and systematized according to the [FURPS+ model](https://en.wikipedia.org/wiki/FURPS).

##### Functionality
1. Each system can only access the data that pertains to it.
2. The integrity of the data accessed by the systems must be audited and verified.
3. All information must be protected from unauthorized access, considering the principles of minimizing access to what is essential for each user/application, creating tunnels for information transfer, evaluating data and application integrity, and data encryption/minimization.
4. Since the order management module is customer-facing, special attention must be paid to data privacy and protection under the GDPR. Therefore, the system must comply with the relevant legislation, provide legal information, and inform the user during registration, as well as allow access and account cancellation under legally permitted conditions.

##### Usability
5. The SPA should provide access to all modules of the system: master data, planning, and visualization, as well as GDPR.


##### Reliability
N/A

##### Performance
N/A

##### Supportability
7. Although it is not within the current project's scope, the architecture of the solution should take into account future extension to mobile applications.

##### Design constraints
8. The system must