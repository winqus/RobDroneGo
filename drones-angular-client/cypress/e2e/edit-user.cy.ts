import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomName } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.user;
const editedUserCreds = USER_CREDS.user;

describe('Edit User Component', () => {
  before(() => {
    const userName = randomName();
    Cypress.env('edit-user_randomUserName', userName);
  });

  beforeEach(() => {
    cy.login(userCreds);
    cy.visit('/');
    cy.visit('auth/user');
    cy.intercept(convertRouteToPath(API_ROUTES.user.update)).as('updateUser');
  });

  it('should navigate to edit user page', () => {
    cy.url().should('include', '/auth/user');
  });

  it('should show the users current information', () => {
    cy.get('#firstName');
    cy.get('#lastName');
    cy.get('#phonenumber');
  });

  it('should update the user when pressing the update user button', () => {
    cy.get('#firstName').clear().type('UpdatedFirstName');
    cy.get('#lastName').clear().type('UpdatedLastName');
    cy.get('#phonenumber').clear().type('119263748');
    cy.contains('Update User').click();
    cy.wait('@updateUser').its('response.statusCode').should('eq', 200);

    // Change back to original values
    cy.get('#firstName').clear().type(userCreds.firstName);
    cy.get('#lastName').clear().type(userCreds.lastName);
    cy.get('#phonenumber').clear().type('119263748');
    cy.contains('Update User').click();
    cy.wait('@updateUser').its('response.statusCode').should('eq', 200);
  });

  it('should update the password', () => {
    cy.contains('Update password').click();
    cy.get('#password').clear().type(userCreds.password);
    cy.get('#confirmPassword').clear().type(userCreds.password);
    cy.contains('Update User').click();
    cy.wait('@updateUser').its('response.statusCode').should('eq', 200);
    cy.login(userCreds);
  });

  it('should call delete user and go to the sign-up page', () => {
    cy.intercept(convertRouteToPath(API_ROUTES.user.delete), { statusCode: 200 }).as('deleteUserData');
    cy.contains('Delete user').click();
    cy.get('#my_modal_1 button.btn').contains('Yes, delete').click();
    cy.wait('@deleteUserData').its('response.statusCode').should('eq', 200);
    cy.url().should('include', '/signup');
  });
});
