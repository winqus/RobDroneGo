import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomName } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { CreateTaskRequestDTO } from 'src/app/services/task-request.service';
import LoginCredentials from './loginCreds.interface';
import { USER_CREDS } from './userCredentials';

export function registerCommands() {
  Cypress.Commands.add('login', (loginCreds: LoginCredentials) => {
    cy.session(
      [loginCreds],
      () => {
        const { email, password } = loginCreds;

        cy.log(`Logging in as ${email}`);

        cy.request({
          method: 'POST',
          url: '/api/auth/signin',
          body: { email, password },
        }).then(({ body }) => {
          window.localStorage.setItem('JWT_TOKEN', body.token);
        });

        cy.wait(100);

        // Save JWT Token after successful login
        cy.window().then((window) => {
          const token = window.localStorage.getItem('JWT_TOKEN');
          Cypress.env('JWT_TOKEN', token); // Store token in Cypress environment
        });
      },
      {
        validate() {
          cy.visit('/');
          cy.url().should('include', '/dashboard');
        },
      },
    );
  });

  Cypress.Commands.add('createBuilding', (buildingCode: string, floorSizeLength = 100, floorSizeWidth = 100) => {
    cy.intercept('POST', '/api/building').as('createBuilding');

    cy.visit('/campus/building/create');

    cy.get('#buildingCode').type(buildingCode);
    cy.get('#buildingName').type('Building' + buildingCode);
    cy.get('#buildingDescription').type('Description of Building ' + buildingCode);
    cy.get('#floorSizeLength').type(floorSizeLength.toString());
    cy.get('#floorSizeWidth').type(floorSizeWidth.toString());

    cy.contains('Create Building').click();
    let buildingId = '';
    cy.wait('@createBuilding');
    // cy.wait('@createBuilding').then((interception) => {
    //   cy.log('awaited createBuilding-----');
    //   expect(interception.response?.statusCode).to.eq(201);
    //   buildingId = interception.response!.body.id;
    // });
    cy.visit('/');
    // return buildingId;
  });

  Cypress.Commands.add('createFloor', (buildingCode: string, floorNumber: number) => {
    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
    cy.intercept('POST', '/api/floor').as('createFloor');

    cy.visit('/campus/floor/create');

    cy.wait('@listBuildings');

    cy.get('#buildingCode').select(buildingCode);
    cy.get('#floorNumber').type(floorNumber.toString());
    cy.get('#floorDescription').type('Floor ' + floorNumber);

    cy.contains('Create Floor').click();
    let floorId: string = '';
    cy.wait('@createFloor').then((interception) => {
      expect(interception.response?.statusCode).to.eq(201);
      floorId = interception.response!.body.id;
    });
    cy.visit('/');
    // return floorId;
  });

  Cypress.Commands.add('createRoom', (buildingCode: string, floorNumber: number, roomName: string, size: [number, number], position: [number, number], category: string = 'Other') => {
    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
    cy.intercept(convertRouteToPath(API_ROUTES.floor.getByBuildingCode(buildingCode))).as('getBuildingFloors');
    cy.intercept(convertRouteToPath(API_ROUTES.room.createRoom)).as('createRoom');

    cy.visit('/campus/room/create');
    cy.url().should('include', '/campus/room/create');

    cy.wait('@listBuildings');
    // cy.wait(100);
    cy.get('#buildingCode').select(buildingCode);
    // cy.wait(100);

    cy.wait('@getBuildingFloors');
    cy.get('#floorNumber').select(`${floorNumber}`);

    cy.get('#roomName').type(roomName);
    cy.get('#roomDescription').type('Room ' + roomName + ' description');

    cy.get('#roomSizeW').type(size[0].toString());
    cy.get('#roomSizeL').type(size[1].toString());
    cy.get('#roomPositionX').type(position[0].toString());
    cy.get('#roomPositionY').type(position[1].toString());
    cy.get('#roomCategory').select('Other');

    cy.contains('Create Room').click();
    cy.wait('@createRoom').its('response.statusCode').should('eq', 201);

    cy.contains('Room Created').should('be.visible');
  });

  /*Cypress.Commands.add('createTaskRequest', (taskRequest: CreateTaskRequestDTO) => {
    cy.login(USER_CREDS.user);

    const route = API_ROUTES.taskRequest.create;
    const postTaskRequestDTO: CreateTaskRequestDTO = {
      requesterEmail: taskRequest.requesterEmail,
      task: taskRequest.task,
    };

    let jwtToken: any = Cypress.env('JWT_TOKEN');
    // cy.window().then((window) => {
    //   jwtToken = window.localStorage.getItem('JWT_TOKEN');
    // });

    cy.log('JWT_TOKEN is ', jwtToken);
    let taskRequestId = '';
    cy.request({
      // url: `http:localhost:4000${convertRouteToPath(route)}`,
      url: convertRouteToPath(route),
      method: 'POST',
      headers: {
        Authorization: `Bearer ${jwtToken}`,
      },
      body: postTaskRequestDTO,
    }).then((response) => {
      expect(response.status).to.eq(201);
      taskRequestId = response.body.id;
    });

    // return taskRequestId;
  });*/

  Cypress.Commands.add('createSurveillanceTaskRequest', (requesterEmail: string, buildingCode: string, floorNumber: number[], contactNumber: number = 123456789) => {
    cy.intercept('/api/building/all').as('getAllBuildings');
    cy.intercept('/api/room/all').as('getAllRooms');

    cy.visit('/task/request-task');

    cy.get('.select').select('Surveillance').should('have.value', 'Surveillance');
    cy.wait('@getAllBuildings');
    cy.wait('@getAllRooms');
    cy.get('.select').eq(1).select(buildingCode).should('have.value', buildingCode);

    for (const number of floorNumber) {
      cy.get('.label-text').contains(number.toString()).click();
    }
    cy.get('.btn').contains('Submit Task Request').click();
    cy.get('small').contains('Task request submitted successfully!');
  });
}
