##Documentation Roadmap and Overview
Purpose and Scope of the SAD
How the SAD Is Organized
How a View Is Documented

#Documentation Roadmap and Overview
This section offers information to assist readers or users of the Software Architecture Document (SAD) in quickly finding the information they need to fulfill their responsibilities. It serves as a starting point for those looking for an overview of the document and those seeking specific information to answer particular questions.

Purpose and Scope of the SAD
This section clarifies the overall purpose and scope of the SAD. It explains the criteria for identifying architectural design decisions (to be documented in the SAD) and non-architectural design decisions (to be documented elsewhere).

The software architecture of a system encompasses its structure, including software elements and their externally visible properties, as well as the relationships between them (Bass 2012).

This SAD describes the software architecture of a system being developed upon the request of the Intermunicipal Transport Authority (ITA) for the management and planning of public transportation. This system enables the management and general public to access information about various transportation networks, routes, and trips. It also facilitates the planning of vehicle services and crew assignments for these routes.

This SAD is developed in an academic teaching and learning context (in the 5th semester of the 2020-2021 academic year). Students are acquiring various competencies throughout the semester while concurrently developing the system.

Given its aim to support the teaching and learning process, this document is not meant to be exhaustive or describe the best possible architecture but rather to serve as a guide and example in line with the competencies to be acquired in each project iteration/sprint.

While students are the primary audience for this SAD, the competencies gained by students in various course units throughout the semester enable them to take on different roles (different stakeholders/recipients), e.g., requirements elicitors, analysts, software architects, programmers/testers, administrators, operators, and users.

How the SAD Is Organized
This section offers a narrative description of the seven major sections of the SAD and outlines the contents of each. Readers in search of specific information can use this section to expedite their search. The SAD is divided into the following seven sections:

Documentation Roadmap and Overview: Provides information about this document and its intended audience. It offers a roadmap and document overview.
Architecture Background: Provides information about the software architecture. It explains the background and rationale behind the software architecture. It discusses the constraints and influences that led to the current architecture and describes the major architectural approaches used.
Views and 4. Mapping Between Views: Both sections specify the software architecture.
Referenced Materials: Provides reference information for documents cited elsewhere in this SAD.
Glossary and Acronyms: Contains an index of architectural elements and relationships along with their definitions. It indicates where each term is used in this SAD.
This DAS/SAD follows the proposed structure mentioned above.

How a View Is Documented
Primary Presentation:

Is usually graphical.
Should include a key that explains the notation.
Shows elements and relations among them.
Presents the most important information about the view first.
Identifies elements that fall outside the scope of the view (if external entities are not clearly marked in the diagram, consider adding a context diagram).
Element Catalog:

Explains elements depicted in the primary presentation and their properties.
Typically presented as a table with element names and textual descriptions.
May include interface documentation.
May include behavior documentation.
Variability Guide:

Identifies points in the system that can be parameterized or reconfigured.
Examples include the number of instances in a pool, support for plug-ins or add-ons, and support for different versions of the OS, database server, or runtime environment.
The view may serve as a reference architecture, providing guidelines for instantiation.
Other Information:

Provides a description and rationale for significant design decisions, including any alternative solutions that were considered and rejected.
Includes the results of analysis, prototypes, and experiments.
May include a context diagram.
Parent View:

If the current view is a refinement of another view, indicate which view that is.
In this DAS/SAD, the UML notation is adopted, including (but not limited to) component diagrams, sequence diagrams, package diagrams, and node diagrams. This choice ensures that requirements 1.1, 1.2, and 1.3 are met.

The organization of views using a combination of the C4 model (different levels of abstraction/granularity) and the 4+1 views model (multiple architectural perspectives) immediately addresses requirement 1.4.

By adopting the C4 model, requirement 1.5 is also addressed.
Pela adoção do modelo C4, o requisito 1.5 é endereçado.