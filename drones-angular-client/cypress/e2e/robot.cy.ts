import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomRobotCode } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.fleetManager;

describe('Robot tests', () => {
    before(() => {
        const robotCode = randomRobotCode();
        Cypress.env('robot_randomRobotCode', robotCode);
    });

    beforeEach(() => {

        cy.login(userCreds);
        cy.visit('/');

        cy.intercept(convertRouteToPath(API_ROUTES.robot.createRobot)).as('createRobot');
        cy.intercept(convertRouteToPath(API_ROUTES.robot.getAll)).as('listRobots');
    });

    it('should navigate to List Robots page', () => {
        cy.visit('/fleet/robot/list');

        cy.wait('@listRobots');

        cy.url().should('include', '/fleet/robot/list');
    });

    it('should show a message if robot does not exist', () => {
        cy.visit('/fleet/robot/list');

        cy.url().should('include', '/fleet/robot/list');

        cy.contains('No robots found').should('be.visible');
    });

    it('should create robot', () => {
        const robotCode = Cypress.env('robot_randomRobotCode');
        cy.visit('/fleet/robot/create');

        cy.get('#robotCode').type('c' + robotCode);
        cy.get('#robotNickname').type('N' + robotCode);
        cy.get('#robotSerialNumber').type('s' + robotCode);
        cy.get('#robotDescription').type('d' + robotCode);
        cy.get('#robotType').type('Delivery');

        cy.contains('Create Robot').click();

        cy.wait('@createRobot').its('response.statusCode').should('eq', 201);
    });

    it('should create and list robot', () => {
        const robotCode = Cypress.env('robot_randomRobotCode');
        cy.visit('/fleet/robot/create');

        cy.get('#robotCode').type(robotCode);
        cy.get('#robotNickname').type('R' + robotCode);
        cy.get('#robotSerialNumber').type('S ' + robotCode);
        cy.get('#robotDescription').type('D ' + robotCode);
        cy.get('#robotType').type('Delivery');

        cy.contains('Create Robot').click();

        cy.wait('@createRobot').its('response.statusCode').should('eq', 201);

        cy.visit('/fleet/robot/list');

        cy.url().should('include', '/fleet/robot/list');

        cy.contains(robotCode).should('be.visible');
    });
});