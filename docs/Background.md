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
1. ID10 As an administrator, I want to create a system user indicating their permissions.
1. ID20 As a potential user of the system (e.g., student, teacher) I intend to register as a user of the system and:
1. ID30 Give my consent or not to the collection and processing of my personal data.
1. ID40 Be informed of the purpose for which personal data is collected and processed.
1. ID50 Know and have access to the person responsible for the treatment.
1. ID60 Know the period for which the data will be kept.
1. ID70 Know my rights as a holder of personal data.
1. ID80 As a system administrator, I want to approve or reject a user's registration.
1. ID100 As a user of the system, I intend to rectify/change my data.
1. ID110 As a user of the system, I want a copy of my personal data.
1. ID120 As a user of the system, I intend to cancel my account ensuring that my data is deleted.
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
1. ID460 As a user of the system, I intend to request a task indicating the parameters necessary for its execution, namely the starting and ending points and the desired task.
1. ID470 As a task manager, I want to approve or reject a request.
1. ID480 As a task manager, I intend to consult task requests that have not yet been approved.
1. ID490 As a task manager, I want to search task requests by status, device type, or user.
1. ID500 As a task manager, I want to obtain the execution sequence of approved tasks.
1. ID601 The base format of the map that describes the floor plan of a campus building (a file/floor/building) must be complemented with information from the campus management module: floor size, arrangement of walls, location of room doors/offices, accesses and elevators, etc.
1. ID602 The base format of the map that describes the plan of a floor of a campus building (a file/floor/building) must define, in addition to the matrix grid, the identification of files containing the appropriate textures for the walls of the different floors, as well as the 3D models representing doors and elevators.
1. ID603 Allow 3D visualization of the interior of the floors of campus buildings (one floor at a time): floor, walls, doors, and elevators.
1. ID604 Create a user interface (GUI) that allows you to select the building and floor you want to see at any given time.
1. ID605 Import the 3D model of a robot and allow it to be viewed in a given location on the selected floor.
1. ID607 When navigating interactively and arriving at a passage between buildings, you should automatically transition to the adjacent floor.
1. ID608 When navigating interactively and entering an elevator, it must be possible to choose the floor you want to access and move to that floor.
1. ID609 Animate the automatic movement of the robot from a given starting point to a given arrival point, according to the information provided by the route planning module.
1. ID610 When navigating automatically based on route planning, passage between buildings must have adequate visual feedback.
1. ID611 When navigating automatically based on route planning, elevator use must have adequate visual feedback.
1. ID612 Display a floating tip that identifies the room, office, or elevator to which the cell pointed to by the mouse cursor belongs.
1. ID680 As an organization administrator, I want a disaster recovery plan that satisfies the MBCO defined in sprint B.
1. ID690 As an administrator of the organization, I want to be presented in a justified manner with the changes to be made to the infrastructure in order to ensure an MTD (Maximum Tolerable Downtime) of 20 minutes.
1. ID720 As a solution architect, I want information about tasks to be shared between the Data Administration and Planning modules.
1. ID730 As a solution architect, I want information about a robot's paths to perform tasks to be shared between the Planning and Visualization modules.
1. ID740 As a solution architect, I intend to use an IAM module (e.g., Azure, auth0, Google, Linkedin) to manage identities and permissions (role-based).
1. ID750 As a solution architect, I intend to use an IAM module (e.g., Azure, auth0, Google, Linkedin) to manage identities and permissions (role-based).
1. ID760 As a solution architect, I want a duly justified and clear diagram of which components will exist in the solution and what their integration interfaces are, indicating the type of information and information structure to be shared.
1. ID840 As a systems administrator, I want a backup copy of the DB(s) to be made to a Cloud environment through a script that renames it to the format <db_name>_yyyymmdd with <db_name> being the name of the database, yyyy the year the copy was made, mm the month the copy was made and dd the day the copy was made.
1. ID850 As a systems administrator, I want a script to be created using the Backup created in US 840 to manage the files resulting from that backup, within the following schedule: 1 Backup per month for the last year, 1 backup per week for the last month, 1 backup per day for the last week.
1. ID860 As a systems administrator, I want the DB backup US process to be kept in the Linux log, in an appropriate context, and the administrator is alerted when accessing the console if a serious failure occurs in this process.
1. ID870 As a systems administrator, I want the US backup copy of the DB backup to have a lifespan of no more than 7 (seven) days, except as indicated in the monthly and annual copy retention US.
1. ID880 As an administrator of the organization, I want a BIA (Business Impact Analysis) of the final solution to be presented to me, adapting if and where applicable the risk(s) identified in the previous sprint.
1. ID890 As an organization administrator, I want access management to be implemented that meets appropriate security criteria.
1. ID900 As an administrator of the organization, I want a clustering system to be implemented in a justified manner between the systems that implement the SPA.
1. ID910 As a systems administrator, I want the administrator to have SSH access to the virtual machine, only through a certificate, without using a password.
1. ID920 As a systems administrator, I want a public file share to be created between the various teams, in SMB/CIFS or NFS format, to speed things up.
1. ID930 As a systems administrator, we must ensure that, if necessary, backups were carried out correctly. To do this, we must automate its replacement, ultimately validating the functioning of the system (Ex. Database - execute a successful SQL query after replacement).
1. ID1300 As a user, I want the application menu to adjust depending on my type of user.
1. ID1310 As a solution architect, I want all backend services to be authenticated.
1. ID1320 As a solution architect, I want all backend services to validate user permissions to perform a certain operation.
1. ID1410 A plan for meeting the indicated tasks is generated, indicating a sequence for executing the tasks that is created by generating all sequences and choosing the one that lasts the shortest time.
1. ID1420 A study of the complexity of the solution to requirement 1410 must be carried out, allowing us to conclude up to what dimension of number of tasks this approach can be used.
1. ID1430 A fulfillment plan for the indicated tasks is generated that indicates a sequence of task execution that is created through a Genetic Algorithm.
1. ID1440 Carry out a study of the state of the art in generating robot trajectories. They will be able to combine their own bibliographic research with Generative AI tools (for example ChatGPT), indicating the prompts used and making it very clear what was produced via Generative AI tools.


#### Quality attributes
Quality attributes are categorized and systematized according to the [FURPS+ model](https://en.wikipedia.org/wiki/FURPS).

##### Functionality
1. Each system can only access the data that pertains to it.
1. The integrity of the data accessed by the systems must be audited and verified.
1. All information must be protected from unauthorized access, considering the principles of minimizing access to what is essential for each user/application, creating tunnels for information transfer, evaluating data and application integrity, and data encryption/minimization.
1. Since the task and user management modules are customer-facing, special attention must be paid to data privacy and protection under the GDPR. Therefore, the system must comply with the relevant legislation, provide legal information, and inform the user during registration, as well as allow access and account cancellation under legally permitted conditions.
1. Support for predefined task types (e.g., surveillance, pickup & delivery).
1. Unique identification for buildings, floors, robots, elevators, and passages.

##### Usability
1. The SPA should provide access to all modules of the system: master data, planning, and visualization, as well as GDPR.
1. Intuitive UI menus for modules like campus management, fleet management, task management, and 3D visualization.
1. Features like pre-filled fields in forms and a straightforward approach to approve or reject task requests.

##### Reliability
1. Backup Management: System for managing backups based on specified criteria and schedules.
1. Clustering for SPA: Implementing clustering for the SPA to avoid single points of failure.

##### Performance
N/A

##### Supportability
7. Although it is not within the current project's scope, the architecture of the solution should take into account future extension to mobile applications.

