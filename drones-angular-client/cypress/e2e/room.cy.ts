import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomBuildingCode, randomFloorNumber, randomRoomName } from 'cypress/utils/randomData';
import { floor } from 'lodash';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.campusManager;

describe('Room tests', () => {
  before(() => {
    cy.login(userCreds);

    const buildingCode = randomBuildingCode();
    Cypress.env('room_randomBuildCode', buildingCode);

    cy.intercept(convertRouteToPath(API_ROUTES.building.create)).as('createBuilding');
    cy.createBuilding(buildingCode, 1000, 1000);
    cy.wait('@createBuilding').its('response.statusCode').should('eq', 201);

    const floorNumber = randomFloorNumber();
    Cypress.env('room_randomFloorNumber', floorNumber);
    cy.createFloor(buildingCode, floorNumber);
  });

  beforeEach(() => {
    cy.login(userCreds);

    cy.intercept(convertRouteToPath(API_ROUTES.room.createRoom)).as('createRoom');
    cy.intercept(convertRouteToPath(API_ROUTES.building.getAll)).as('listBuildings');
    cy.intercept(convertRouteToPath(API_ROUTES.floor.getByBuildingCode(Cypress.env('room_randomBuildCode')))).as('getBuildingFloors');
  });

  describe('Navigation to Create Room Page', () => {
    it('should navigate to Room Create page', () => {
      cy.visit('/campus/room/create');

      cy.url().should('include', '/campus/room/create');

      cy.contains('Create Room').should('have.attr', 'disabled');
    });
  });

  describe('Room Creation', () => {
    it('should submit the Room Create form', () => {
      const buildingCode = Cypress.env('room_randomBuildCode');
      const floorNumber = Cypress.env('room_randomFloorNumber');

      cy.visit('/campus/room/create');
      cy.url().should('include', '/campus/room/create');

      cy.wait('@listBuildings');
      cy.get('#buildingCode').select(buildingCode);

      cy.wait('@getBuildingFloors');
      cy.get('#floorNumber').select(`${floorNumber}`);
      const roomName = randomRoomName();
      cy.get('#roomName').type(roomName);
      cy.get('#roomDescription').type('Room ' + roomName + ' description');

      cy.get('#roomSizeW').type('10');
      cy.get('#roomSizeL').type('10');
      cy.get('#roomPositionX').type('1');
      cy.get('#roomPositionY').type('1');
      cy.get('#roomCategory').select('Other');

      cy.contains('Create Room').click();
      cy.wait('@createRoom').its('response.statusCode').should('eq', 201);

      cy.contains('Room Created').should('be.visible');
    });

    it('should show error when submitting the same room twice', () => {
      const buildingCode = Cypress.env('room_randomBuildCode');
      const floorNumber = Cypress.env('room_randomFloorNumber');

      cy.visit('/campus/room/create');
      cy.url().should('include', '/campus/room/create');

      cy.wait('@listBuildings');
      cy.get('#buildingCode').select(buildingCode);

      cy.wait('@getBuildingFloors');
      cy.get('#floorNumber').select(`${floorNumber}`);
      const roomName = randomRoomName();
      cy.get('#roomName').type(roomName);
      cy.get('#roomDescription').type('Room ' + roomName + ' description');

      cy.get('#roomSizeW').type('10');
      cy.get('#roomSizeL').type('10');
      cy.get('#roomPositionX').type('12');
      cy.get('#roomPositionY').type('12');
      cy.get('#roomCategory').select('Other');

      cy.contains('Create Room').click();
      cy.wait('@createRoom').its('response.statusCode').should('eq', 201);

      cy.contains('Room Created').should('be.visible');

      cy.contains('Create Room').click();
      cy.wait('@createRoom').its('response.statusCode').should('eq', 400);
      cy.contains('Room already exists').should('be.visible');
    });

    it('should show error when creating an overlapping room', () => {
      const buildingCode = Cypress.env('room_randomBuildCode');
      const floorNumber = Cypress.env('room_randomFloorNumber');

      cy.visit('/campus/room/create');
      cy.url().should('include', '/campus/room/create');

      cy.wait('@listBuildings');
      cy.get('#buildingCode').select(buildingCode);

      cy.wait('@getBuildingFloors');
      cy.get('#floorNumber').select(`${floorNumber}`);
      const roomName = randomRoomName();
      cy.get('#roomName').type(roomName);
      cy.get('#roomDescription').type('Room ' + roomName + ' description');

      cy.get('#roomSizeW').type('10');
      cy.get('#roomSizeL').type('10');
      cy.get('#roomPositionX').type('3');
      cy.get('#roomPositionY').type('3');
      cy.get('#roomCategory').select('Other');

      cy.contains('Create Room').click();
      cy.wait('@createRoom').its('response.statusCode').should('eq', 400);
      cy.contains('Room overlaps with another room').should('be.visible');
    });
  });
});
