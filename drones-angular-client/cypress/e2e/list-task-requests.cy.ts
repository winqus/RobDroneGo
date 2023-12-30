import { any } from 'cypress/types/bluebird';
import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode, randomFloorNumber, randomName, randomRoomName } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.taskManager;

describe('List task requests', () => {
  before(() => {
    cy.login(USER_CREDS.campusManager);

    const buildingCode = randomBuildingCode();
    Cypress.env('listTaskRequest_randomBuildCode', buildingCode);
    cy.createBuilding(buildingCode, 10000, 10000);

    const floorNumber = randomFloorNumber();
    Cypress.env('listTaskRequest_randomFloorNumber', floorNumber);
    cy.createFloor(buildingCode, floorNumber);

    // const roomName = randomRoomName();
    // Cypress.env('listTaskRequest_randomRoomName', roomName);
    // cy.createRoom(buildingCode, floorNumber, roomName, [3, 3], [0, 0]);

    // const roomName2 = randomRoomName();
    // Cypress.env('listTaskRequest_randomRoomName2', roomName2);
    // cy.createRoom(buildingCode, floorNumber, roomName2, [3, 3], [5, 5]);

    cy.login(USER_CREDS.user);
    cy.createSurveillanceTaskRequest(userCreds.email, buildingCode, [floorNumber]);
    cy.createSurveillanceTaskRequest(userCreds.email, buildingCode, [floorNumber]);
    cy.createSurveillanceTaskRequest(userCreds.email, buildingCode, [floorNumber]);
  });

  beforeEach(() => {
    cy.login(userCreds);

    cy.intercept(convertRouteToPath(API_ROUTES.taskRequest.getAll)).as('listTaskRequests');
  });

  it('should navigate to task list page', () => {
    cy.visit('/task/list');
    cy.url().should('include', 'task/list');
  });

  it('should list at least 3 pending task request', () => {
    cy.visit('/task/list');
    cy.wait('@listTaskRequests').its('response.statusCode').should('eq', 200);

    cy.get('.collapse').filter(':contains("pending")').its('length').should('be.at.least', 3);
  });

  it('should approve a pending task request', () => {
    cy.intercept({ url: convertRouteToPath(API_ROUTES.taskRequest.updateStatus('*')), method: 'PATCH' }).as('approveTaskRequest');

    cy.visit('/task/list');
    cy.wait('@listTaskRequests');

    const pendingTaskCollapseElement = cy.get('.collapse').filter(':contains("pending")').eq(0);
    pendingTaskCollapseElement.click();
    pendingTaskCollapseElement.get('.btn-success').contains('Approve').click();

    cy.wait('@approveTaskRequest').its('response.statusCode').should('eq', 200);
  });

  it('should reject a pending task request', () => {
    cy.intercept({ url: convertRouteToPath(API_ROUTES.taskRequest.updateStatus('*')), method: 'PATCH' }).as('rejectTaskRequest');

    cy.visit('/task/list');
    cy.wait('@listTaskRequests');

    const pendingTaskCollapseElement = cy.get('.collapse').filter(':contains("pending")').eq(0);
    pendingTaskCollapseElement.click();
    pendingTaskCollapseElement.get('.btn-error').contains('Deny').click();

    cy.wait('@rejectTaskRequest').its('response.statusCode').should('eq', 200);
  });

  it('should only list pending task requests', () => {
    cy.visit('/task/list');
    cy.wait('@listTaskRequests');

    cy.contains('Show only pending requests').click();

    cy.get('.collapse').filter(':contains("pending")').its('length').should('be.at.least', 1);
    cy.get('.collapse').filter(':contains("approved")').should('not.exist');
    cy.get('.collapse').filter(':contains("denied")').should('not.exist');
  });
});
