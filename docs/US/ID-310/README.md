# ID 310 - Create building floor room

## Description
Implement a POST method for creating a room on a specified floor.

Room attributes: Code, category (one of these: Office, Amphitheater, Laboratory, Other), description (max. 255 chararacters), could have position and size.

## Acceptance Criteria

* POST method can be called to create an room.
* Appropriate HTTP Status codes are returned (e.g. 201 / 202 / 400).
* Unit tests are written and passed.

## Questions from the forum

### Discussion - "US id310/190/230 Create Building Floor Roomm"
Creation date: 09/10/2023. [Go to discussion.](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25016)

**Discussion summary:**
US310 facilitates the creation of a room within a building, allowing for designation of the room's name, floor location, category (such as Office, Amphitheater, Laboratory, or Other), and a brief description, with room position and dimensions being specified during the creation process or included within an uploaded file.

### Discussion - "US310"
Creation date: 19/10/2023 [Go to discussion.](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25188)

**Discussion summary:**
The name of the room is provided by the user at the time of creation.

### Discussion - "US310- Create building floor room"
Creation date: 19/10/2023 [Go to discussion.](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25276)

**Discussion summary:**
The room name must be unique.

### Discussion - "\[310] - Create building floor room"
Creation date: 21/10/2023 [Go to discussion.](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25332)

**Discussion summary:**
Room attributes must adhere to the following constraints: the name is limited to a maximum of 50 characters, the description can be up to 250 characters, and the minimum room size is set at 1 cell.

## Diagrams

### Logical View Lv1
![LV Lv1](../../diagrams/level1/Logical%20View%20Lv1.svg)

### Logical View Lv2
![LV Lv2](../../diagrams/level2/Logical%20View%20Lv2.svg)

### Logical View Lv3
![LV Lv3](../../diagrams/level3/Logical%20View%20lv3%20(Campus%20Management).svg)

### Deployment View
![DV Lv1](../../diagrams/Deployment%20View.svg)

### Process Diagram Lv1
![SD Lv1](./SD%20Lv1.svg)

### Process Diagram Lv2
![SD Lv2](./SD%20Lv2.svg)

### Process Diagram Lv3
![SD Lv3](./SD%20Lv3.svg)

### Domain Model
![DM](../../diagrams/DM.svg)

## Observations