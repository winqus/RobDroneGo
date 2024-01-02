import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.campusManager;

// Data used:
// Create a building with code Y

describe('Building List tests', () => {
  before(() => {
    const buildingCode = randomBuildingCode();
    Cypress.env('building_randomBuildCode', buildingCode);
  });

  beforeEach(() => {
    // Any preparation before each test
    cy.login(userCreds);
    cy.visit('/');

    // Add any intercepts (for backend waiting) needed for the tests, you can add them here or in the tests themselves.
    // E.g.: Use with cy.wait('@createBuilding') to wait for the request to finish
    cy.intercept(convertRouteToPath(API_ROUTES.building.create)).as('createBuilding');
    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
  });

  it('should navigate to List Buildings page', () => {
    cy.visit('/campus/building/list');

    // For some reason returns 304 instead of 200, but list is still returned correctly. As such not checking status code.
    // cy.wait('@listBuildings').its('response.statusCode').should('eq', 200);
    cy.wait('@listBuildings');

    cy.url().should('include', '/campus/building/list');
  });

  it('should show a message if building does not exist', () => {
    cy.visit('/campus/building/list');

    cy.url().should('include', '/campus/building/list');

    cy.contains('No buildings were created yet').should('be.visible');
  });

  it('should list buildings', () => {
    const buildingCode = Cypress.env('building_randomBuildCode');
    cy.visit('/campus/building/create');

    cy.get('#buildingCode').type(buildingCode);
    cy.get('#buildingName').type('Building' + buildingCode);
    cy.get('#buildingDescription').type('Building ' + buildingCode);
    cy.get('#floorSizeLength').type('10');
    cy.get('#floorSizeWidth').type('10');

    cy.contains('Create Building').click();

    // Wait for request to finish, @createBuilding was defined in beforeEach
    // cy.wait('@createBuilding').its('response.statusCode').should('eq', 201);
    // Got annoyed by hot test reloads, so I'm just checking for 201 or 400
    // cy.wait('@createBuilding').its('response.statusCode').should('be.oneOf', [201, 400]);
    cy.wait('@createBuilding').its('response.statusCode').should('eq', 201);

    cy.visit('/campus/building/list');

    cy.url().should('include', '/campus/building/list');

    // For some reason returns 304 instead of 200, but list is still returned correctly. As such not checking status code.
    // cy.wait('@listBuildings').its('response.statusCode').should('eq', 200);
    cy.wait('@listBuildings');

    cy.contains('Code: ' + buildingCode).should('be.visible');
  });
});
