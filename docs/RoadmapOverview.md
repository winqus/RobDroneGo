- ## Contents
  - [Documentation Roadmap and Overview](#documentation-roadmap-and-overview)
    - [Purpose and Scope of the SAD](#purpose-and-scope-of-the-sad)
    - [How the SAD Is Organized](#how-the-sad-is-organized)
    - [How a View Is Documented](#how-a-view-is-documented)

# Documentation Roadmap and Overview

> Sub-sections in this section provide information to assist readers or users of the Software Architecture Document (SAD) in quickly finding information necessary for their tasks. Readers of the SAD seeking an overview should begin here, as should readers interested in finding specific information to address particular questions.

# Purpose and Scope of the SAD

> This section explains the overall purpose and scope of the SAD, outlining the criteria for determining which design decisions are architectural (and thus documented in the SAD) and which design decisions are non-architectural (and therefore documented elsewhere).

The software architecture of a system is the structure or structures of that system, which includes software elements and their externally visible properties, as well as the relationships between them (Bass 2012).

This SAD describes the software architecture of the system being developed at the request of the client for the management and planning of campus facilities and robots. This system allows management of buildings, floors, rooms, robots, tasks, users.

This SAD is developed in an academic context of teaching and learning (in the 5th semester of the 2023-2024 academic year), where various competencies are being acquired throughout the semester by the students, while they simultaneously develop the system.

As it aims to support the teaching and learning process, it is not intended to be comprehensive or describe the best possible architecture. Instead, it serves as a guide and example, aligned with the competencies to be acquired in each iteration/sprint of the project.

Although students are the primary audience for this SAD, the competencies acquired by students in various course units throughout the semester enable them to assume different roles (different stakeholders/recipients), e.g., requirements elicitors, analysts, software architects, programmers/testers, administrators, operators (ops), and users.

# How the SAD Is Organized

> This section provides a narrative description of the seven major sections of the SAD and their overall contents. Readers seeking specific information can use this section to expedite their search. This SAD is organized into the following seven sections:

1. [Documentation Roadmap and Overview](#documentation-roadmap-and-overview): Provides information about this document and its intended audience. It offers the roadmap and document overview.
2. [Architecture Background](#architecture-background): Provides information about the software architecture. It describes the background and rationale for the software architecture. It explains the constraints and influences that led to the current architecture and describes the major architectural approaches used.
3. [Views](#views) and [Mapping Between Views](#mapping-between-views): Both sections specify the software architecture.
4. [Referenced Materials](#referenced-materials): Provides reference information for documents cited elsewhere in this SAD.
5. [Glossary and Acronyms](#glossary-and-acronyms): Contains an index of architectural elements and relationships along with their definitions and indicates where each term is used in this SAD.

This DAS/SAD follows the proposed structure above.

# How a View Is Documented

**Primary Presentation:**
- Is usually graphical.
- Should include a key that explains the notation.
- Shows elements and relations among them.
- Presents the most important information about the view first.
- Identifies elements that fall outside the scope of the view (if external entities are not clearly marked in the diagram, consider adding a context diagram).

**Element Catalog:**
- Explains elements depicted in the primary presentation and their properties.
- Typically presented as a table with element names and textual descriptions.
- May include interface documentation.
- May include behavior documentation.

**Variability Guide:**
- Identifies points in the system that can be parameterized or reconfigured. Examples include the number of instances in a pool, support for plug-ins or add-ons, and support for different versions of the OS, database server, or runtime environment.
- The view may serve as a reference architecture, providing guidelines for instantiation.

**Other Information:**
- Provides a description and rationale for significant design decisions, including any alternative solutions considered and rejected.
- Includes the results of analysis, prototypes, and experiments.
- May include a context diagram.

**Parent View:**
- If the current view refines another view, indicate which view that is.

In this DAS/SAD, the UML notation is adopted, including (but not limited to) component diagrams, sequence diagrams, package diagrams, and node diagrams. This choice ensures that requirements 1.1, 1.2, and 1.3 are met.

The organization of views, using a combination of the C4 model (different levels of abstraction/granularity) and the 4+1 views model (multiple architectural perspectives), immediately addresses requirement 1.4.