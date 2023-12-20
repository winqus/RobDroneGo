import { any } from 'cypress/types/bluebird';
import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode, randomFloorNumber, randomName } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.taskManager;

describe('Search Task Component', () => {
  before(() => {
    cy.login(USER_CREDS.campusManager);

    const buildingCode = randomBuildingCode();
    cy.createBuilding(buildingCode, 10000, 10000);
    const floorNumber = randomFloorNumber();
    const floorId = cy.createFloor(buildingCode, floorNumber);

    cy.login(userCreds);
    cy.createSurveillanceTaskRequest(userCreds.email, buildingCode, [floorNumber]);
  });

  beforeEach(() => {
    cy.login(userCreds);
    cy.visit('task/search');
    cy.intercept('GET', 'task/search').as('getTaskRequests');
  });

  it('should navigate to search task page', () => {
    cy.url().should('include', 'task/search');
  });

  it('should search for tasks and display results', () => {
    cy.get('#status').select('Pending');
    cy.get('#user').type(userCreds.email);
    cy.get('form').submit();
    cy.get('.collapse-title').should('exist');
    cy.get('.collapse-content').first().should('contain', 'Status:');
    cy.get('.collapse-content').first().should('contain', 'User:');
    cy.get('.collapse-content').first().should('contain', 'Robottype:');
  });
});
