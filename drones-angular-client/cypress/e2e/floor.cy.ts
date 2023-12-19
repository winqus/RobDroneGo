import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode, randomFloorNumber } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.campusManager;

// Data used:
// Create a building with code A
// Create a floor with building code A and floor number 10
describe('Floor tests', () => {
  before(() => {
    // Any preparations before running the tests
    cy.login(userCreds);

    const buildingCode = randomBuildingCode();
    Cypress.env('floor_randomBuildCode', buildingCode);

    // cy.visit('/campus/building/create');
    // cy.get('#buildingCode').type(buildingCode);
    // cy.get('#buildingName').type('Building' + buildingCode);
    // cy.get('#buildingDescription').type('Description of Building ' + buildingCode);
    // cy.get('#floorSizeLength').type('10');
    // cy.get('#floorSizeWidth').type('10');
    cy.intercept(convertRouteToPath(API_ROUTES.building.create)).as('createBuilding');
    cy.createBuilding(buildingCode, 10, 10);
    cy.wait('@createBuilding').its('response.statusCode').should('eq', 201);

    // cy.contains('Create Building').click();
  });

  beforeEach(() => {
    // Any preparation before each test
    cy.login(userCreds);

    // Add any intercepts (for backend waiting) needed for the tests, you can add them here or in the tests themselves.
    cy.intercept(convertRouteToPath(API_ROUTES.floor.createFloor)).as('createFloor');
    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
  });

  describe('Navigation to Create Floor Page', () => {
    it('should navigate to Floor Create page', () => {
      cy.visit('/campus/floor/create');

      cy.url().should('include', '/campus/floor/create');

      cy.contains('Create Floor').should('have.attr', 'disabled');
    });
  });

  describe('Floor Creation', () => {
    it('should submit the Floor Create form', () => {
      const buildingCode = Cypress.env('floor_randomBuildCode');
      cy.visit('/campus/floor/create');

      cy.url().should('include', '/campus/floor/create');

      // Fill out the Robot Create form
      cy.get('#buildingCode').select(buildingCode);
      cy.get('#floorNumber').type('10');
      cy.get('#floorDescription').type('Floor 1');

      // Submit the form
      cy.contains('Create Floor').click();
      cy.wait('@createFloor').its('response.statusCode').should('be.oneOf', [201, 400]);

      // Assert that a success message is displayed
      cy.contains('Floor created successfully').should('be.visible');
    });

    it('should block submit button if description is too long', () => {
      const buildingCode = Cypress.env('floor_randomBuildCode');
      cy.visit('/campus/floor/create');
      cy.url().should('include', '/campus/floor/create');

      // Fill out the form
      cy.get('#buildingCode').select(buildingCode);
      // cy.get('#floorDescription').type('Floor 1'.repeat(100));
      cy.get('#floorDescription').invoke('val', 'Floor 1'.repeat(100)).trigger('input');
      cy.get('#floorDescription').type('x');

      cy.get('#floorNumber').type('10');

      cy.contains('Create Floor').should('have.attr', 'disabled');

      // Assert that a validation error message is displayed
      cy.contains('Max 255 characters allowed.').should('be.visible');
    });
  });

  describe('List floors of the building', () => {
    it('should list floors of the building', /*{ scrollBehavior: false },*/ () => {
      //  { scrollBehavior: 'center' }
      const buildingCode = Cypress.env('floor_randomBuildCode');
      cy.intercept(convertRouteToPath(API_ROUTES.floor.getByBuildingCode(buildingCode))).as('getBuildingFloors');

      cy.visit('/campus/building/list');
      cy.url().should('include', '/campus/building/list');
      cy.wait('@listBuildings');

      cy.wait(1000);
      cy.scrollTo(0, 0);

      const buildingItem = cy.get('app-building-list > .bg-base-300').contains(`Code: ${buildingCode}`, { timeout: 1000 });
      buildingItem.click({ force: true, scrollBehavior: false });
      buildingItem.parent().parent().contains('See Floors', { timeout: 1000 }).click({ force: true, scrollBehavior: false });
      cy.url().should('include', `/campus/building/${buildingCode}/floors`);
      cy.wait('@getBuildingFloors');

      cy.contains('Floor Number: 10').should('be.visible');
    });

    it('should show a message if building does not exist', () => {
      cy.visit('/campus/building/BAD/floors');

      cy.url().should('include', '/campus/building/BAD/floors');

      cy.contains('exist or has no floors yet').should('be.visible');
    });
  });
});
