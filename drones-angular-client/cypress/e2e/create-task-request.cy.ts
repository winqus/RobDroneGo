import { any } from 'cypress/types/bluebird';
import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode, randomFloorNumber, randomName, randomRoomName } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.taskManager;

describe('Create task request', () => {
  before(() => {
    cy.login(USER_CREDS.campusManager);

    const buildingCode = randomBuildingCode();
    Cypress.env('createTaskRequest_randomBuildCode', buildingCode);
    cy.createBuilding(buildingCode, 10000, 10000);

    const floorNumber = randomFloorNumber();
    Cypress.env('createTaskRequest_randomFloorNumber', floorNumber);
    cy.createFloor(buildingCode, floorNumber);

    const roomName = randomRoomName();
    Cypress.env('createTaskRequest_randomRoomName', roomName);
    cy.createRoom(buildingCode, floorNumber, roomName, [3, 3], [0, 0]);

    const roomName2 = randomRoomName();
    Cypress.env('createTaskRequest_randomRoomName2', roomName2);
    cy.createRoom(buildingCode, floorNumber, roomName2, [3, 3], [5, 5]);
  });

  beforeEach(() => {
    cy.login(userCreds);

    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
    cy.intercept(convertRouteToPath(API_ROUTES.floor.getByBuildingCode(Cypress.env('createTaskRequest_randomBuildCode')))).as('getBuildingFloors');
    cy.intercept(convertRouteToPath(API_ROUTES.taskRequest.create)).as('createTaskRequest');
  });

  it('should navigate to create task page', () => {
    cy.visit('/task/request-task');
    cy.url().should('include', 'task/request-task');
    cy.get('.select').contains('--Choose Task Type--').should('exist');
  });

  it('should create surveillance task request', () => {
    cy.visit('/task/request-task');

    cy.get('.select').select('Surveillance');
    cy.wait('@listBuildings');
    cy.get('select').eq(1).select(Cypress.env('createTaskRequest_randomBuildCode'));
    cy.wait('@getBuildingFloors');

    cy.get('.cursor-pointer > .label-text').contains(Cypress.env('createTaskRequest_randomFloorNumber').toString()).click();

    cy.get('.input').invoke('val').should('match', /\d{9}/);

    cy.get('.btn').contains('Submit Task Request').click();

    cy.wait('@createTaskRequest').its('response.statusCode').should('eq', 201);
    cy.get('small').contains('Task request submitted successfully!').should('exist');

    cy.get('.mt-6').contains('Submit Task Request').should('be.disabled');
  });

  it('should create delivery task request', () => {
    cy.visit('/task/request-task');

    cy.get('.select').select('Delivery');

    cy.wait('@listBuildings');

    cy.get('[formcontrolname="pickUpBuildingCode"]').select(Cypress.env('createTaskRequest_randomBuildCode'));
    cy.get('[formcontrolname="pickUpFloorNumber"]').select(Cypress.env('createTaskRequest_randomFloorNumber').toString());
    cy.get('[formcontrolname="pickUpRoomId"]').select(Cypress.env('createTaskRequest_randomRoomName'));

    cy.get('[formcontrolname="deliveryBuildingCode"]').select(Cypress.env('createTaskRequest_randomBuildCode'));
    cy.get('[formcontrolname="deliveryFloorNumber"]').select(Cypress.env('createTaskRequest_randomFloorNumber').toString());
    cy.get('[formcontrolname="deliveryRoomId"]').select(Cypress.env('createTaskRequest_randomRoomName2'));

    cy.get('[formcontrolname="pickUpName"]')
      .invoke('val')
      .should('contain', userCreds.firstName + ' ' + userCreds.lastName);
    cy.get('[formcontrolname="deliveryName"]').type(randomName());
    cy.get('[formcontrolname="pickUpContact"]').invoke('val').should('match', /\d{9}/);
    cy.get('[formcontrolname="deliveryContact"]').type('123456789');
    cy.get('[formcontrolname="confirmationCode"]').type('2333');
    cy.get('[formcontrolname="description"]').type('Some description');

    cy.get('.mt-6').contains('Submit Task Request').click();
    cy.wait('@createTaskRequest').its('response.statusCode').should('eq', 201);
    cy.get('small').contains('Task request submitted successfully!').should('exist');
    cy.get('.mt-6').contains('Submit Task Request').should('be.disabled');
  });
});
