import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.user;

describe('Login Test', () => {
  before(() => {
    cy.login(userCreds);
  });

  // Note: At the time not needed, leaving for future reference
  // beforeEach(() => {
  //   // Set JWT Token in local storage before each test
  //   cy.window().then((window) => {
  //     if (Cypress.env('token')) {
  //       window.localStorage.setItem('JWT_TOKEN', Cypress.env('token'));
  //     }
  //   });
  // });

  it('should load the dashboard page', () => {
    cy.visit('/');
    cy.wait(100);

    cy.url().should('include', '/dashboard');

    cy.title().should('include', 'RobDroneGo');

    cy.get('.navbar').contains(userCreds.firstName, { timeout: 500 }).should('be.visible');
  });
});
