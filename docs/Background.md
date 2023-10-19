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
> This section describes the general function and purpose for the system or subsystem whose architecture is described in this SAD.

The Intermunicipal Transport Authority (AIT) wants a public transportation management and planning system that allows both management and public access to different transportation networks, routes, trips, as well as planning of vehicle services and crew assignments on those routes.

### Context
> This section describes the goals and major contextual factors for the software architecture. The section includes a description of the role software architecture plays in the life cycle, the relationship to system engineering results and artifacts, and any other relevant factors.

Transport planning involves various optimizations and resource allocations with the aim of fulfilling the desired transportation service and maximizing certain operator parameters (e.g., STCP) for operational and financial efficiency. In general, the transportation offering of an operator is the set of trips offered by each of its lines along the routes of the network that are most important in terms of people's mobility in the geographic area in which it operates.

*NB: However, the system requested here is a simplification of what a transportation management system is, and simplifications are assumed to make the project feasible in this context (i.e., 5th semester of LEI).*

In this edition of the 5th semester of LEI, we will have the collaboration of the company OPT - Optimização e Planeamento de Transportes, S.A. (http://opt.pt/), which operates in the field of transportation optimization and planning, and develops computer systems that address the issues treated in this project.

This SAD serves as a basis for a discussion of the system to be built (implemented, tested, and deployed) and is intended to be aligned with the constructed system. In addition to the obvious in describing software architecture, it should identify design alternatives and points of variation.

Although students are the primary audience for this SAD, the competencies acquired by students in various course units throughout the semester enable them to assume different roles (different stakeholders/recipients), e.g., requirements elicitors, analysts, software architects, programmers/testers, administrators, operators (ops), and users.

### Driving Requirements
> This section lists the functional requirements, quality attributes, and design constraints. It may point to a separate requirements document.

#### Functional requirements
1. As a data administrator, I want to import nodes, routes, lines, vehicle types, and crew types from a .glx file.
2. As a data administrator, I want to create network nodes, indicating their name, whether they are a collection point or drop-off point, and their coordinates.
3. As a data administrator, I want to create a line, indicating its code (e.g., "C"), name (e.g., "Green Line"), and its terminal nodes (e.g., Campanha, ISMAI), as well as any restrictions on the type of vehicle and crew type.
4. As a data administrator, I want to define a one-way/round-trip route for a line. Define the various segments that make up a route, indicating the order, distance, and travel time of each segment.

[...]

19. As a data administrator, I want to list vehicle service on a specific day.
20. As a data administrator, I want to list crew service on a specific day.

[...]

21. As a customer or manager, I want to view the transportation network graphically in 2D.
22. As a customer or manager, I want to control the view. Add camera pan and zoom commands; the orbit command should be inactive (suggestion: right mouse button - pan; mouse wheel - zoom).
23. As a customer or manager, I want to have a georeferenced representation of the network. Overlay the geographic area (map) covered by the transportation network.

[...]

29. As a customer, I want to access and cancel my account under the legally permitted cases and conditions.

[...]

30. As a customer or manager, I want to control the view. Add interaction objects (e.g., buttons, sliders, etc.) that allow configuring the scene's lighting conditions; these objects should be active in 3D mode and inactive or invisible in 2D mode.
31. As a customer or manager, I want to increase the realism of first-person navigation. Detect vehicle collisions with 3D models representing network nodes.

[...]

<insert use case model here/>

#### Quality attributes
Quality attributes are categorized and systematized according to the [FURPS+ model](https://en.wikipedia.org/wiki/FURPS).

##### Functionality
1. Each system can only access the data that pertains to it.
2. The integrity of the data accessed by the systems must be audited and verified.
3. All information must be protected from unauthorized access, considering the principles of minimizing access to what is essential for each user/application, creating tunnels for information transfer, evaluating data and application integrity, and data encryption/minimization.
4. Since the order management module is customer-facing, special attention must be paid to data privacy and protection under the GDPR. Therefore, the system must comply with the relevant legislation, provide legal information, and inform the user during registration, as well as allow access and account cancellation under legally permitted conditions.

##### Usability
5. The SPA should provide access to all modules of the system: master data, planning, and visualization, as well as GDPR.
6. In the scope of the current project, user administration can be done directly in the database, and a user management module is not necessary.

##### Reliability
N/A

##### Performance
N/A

##### Supportability
7. Although it is not within the current project's scope, the architecture of the solution should take into account future extension to mobile applications.

##### Design constraints
8. The system must