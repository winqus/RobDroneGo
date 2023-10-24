# System Entity-Attribute Specification #

***Alphanumeric** in most general cases (like description) means that spaces are also allowed.*

### Building Attributes ###

* **Code**: Mandatory and unique, max 5 chars, alphanumeric.
* **Name**: Optional, max 50 chars, alphanumeric.
* **Description**: Optional, max 255 chars.
* **Maximum Size of Each Floor**: Defined in terms of cells.

### Floor Attributes ###

* **Building Reference**: Must be pre-existing.
* **Floor Definition**: Number and description.
* **Description**: Optional, max 250 chars.
* **Multiple Passages**: Can have several passages to different buildings.

### Elevator Attributes ###

* **Building**: Mandatory reference.
* **Identification Number**: Mandatory, unique in the building. (ID is based on elevator count in building, starting with ID=1 - if there were more elevators in the future).
* **List of Floors Served**: Mandatory.
* **Make and Model**: Optional, alphanumeric, max 50 chars.
* **Manufacturer’s Serial Number**: Optional, alphanumeric, max 50 chars.
* **Description**: Optional, alphanumeric, max 250 chars.

### Room Attributes ###

* **Name**: Unique, max 50 chars., mandatory.
* **Description**: Max 250 chars., optional
* **Size**: Defined by a minimum of 1 cell.
* **Position and Dimensions**: Can be collected during creation or part of an upload.
* **Category**: Office, Amphitheater, Laboratory, Other.

### Passageway Attributes ###
* **Buildings and floors** that are connected by this passage 
* **Editing**: All data editable except building reference.
* **Multiple Entries**: Multiple passageways can exist between buildings.

### Robot Attributes ###

* **ID Code**: Mandatory, unique, alphanumeric, max 30 chars.
* **Nickname**: Mandatory, unique, alphanumeric, max 30 chars.
* **Type of Robot**: Mandatory.
* **Brand and Model**: Free text attributes.
* **Serial Number**: Mandatory, unique for each robot type, alphanumeric, max 50 chars.
* **Description**: Optional, alphanumeric, max 250 chars.

### Task Attributes ###

* **Types**: Surveillance and Pickup & Delivery.
* **Task Details**: Including locations, contacts, and descriptions.
* **Assignment**: Managed manually by a Task Manager.

### General System Attributes ###

* **User Roles**: Different roles like Task Manager, System Administrator, etc.
* **Modules**: Including Device Management, Task Request Management, etc.
* **Robot Movement**: Orthogonal (up, down, left, right).

### Map and Layout ###

* **Upload Requirements**: Validations including structure and size.
* **Elements**: Including elevators, passages, and rooms.

### Constraints and Assumptions ###

* **Data Constraints**: Various constraints like character limits and mandatory fields.
* **System Constraints**: Such as one elevator per building.
* **Assumptions**: For instance, user input is coherent, and rooms are rectangular.