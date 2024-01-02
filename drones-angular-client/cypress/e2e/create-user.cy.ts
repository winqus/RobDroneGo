import convertRouteToPath from 'cypress/utils/convertRoute';
import { randomEmail } from 'cypress/utils/randomData';
import { API_ROUTES } from 'src/api.config';
import { USER_CREDS } from '../support/userCredentials';

const userCreds = USER_CREDS.systemAdmin;

describe('Create User Component', () => {
  before(() => {
    const email = randomEmail();
    Cypress.env('create-user_randomEmail', email);
  });
  beforeEach(() => {
    cy.login(userCreds);
    cy.visit('/');
    cy.visit('system/create-user');
    cy.intercept(convertRouteToPath(API_ROUTES.user.register)).as('createUser');
  });

  it('should navigate to create user page', () => {
    cy.url().should('include', 'system/create-user');
  });

  it('should create a new user successfully', () => {
    const email = randomEmail();
    Cypress.env('create-user_randomEmail', email);

    cy.get('#firstName').type('John');
    cy.get('#lastName').type('Doe');
    cy.get('#email').type(email);
    cy.get('#phonenumber').type('123456789');
    cy.get('#password').type('Password@123');
    cy.get('#confirmPassword').type('Password@123');
    cy.get('#role').select('Campus Manager');

    cy.get('form').submit();

    cy.wait('@createUser');
  });

  it('should display validation errors for invalid data', () => {
    cy.visit('/');
    cy.visit('system/create-user');

    cy.get('#firstName').type(' ');
    cy.get('#lastName').type(' ');
    cy.get('#email').type(' ');
    cy.get('#phonenumber').type(' ');
    cy.get('#password').type(' ');
    cy.get('#confirmPassword').type(' ');
    cy.get('form').submit();

    cy.get('.text-error').should('contain', 'First Name must be between 2 and 50 characters long');
    cy.get('.text-error').should('contain', 'Last Name must be between 2 and 50 characters long');
    cy.get('.text-error').should('contain', 'This field is required');
    cy.get('.text-error').should('contain', 'Invalid contact number, must be 9 numbers');
    cy.get('.text-error').should('contain', 'Password must contain at least a capital letter, a lowercase letter, a digit and a symbol.');
  });

  it('should display an error message for duplicate email', () => {
    cy.intercept('POST', '/api/createUser', { statusCode: 409, body: 'Email already exists' }).as('createUser');
    const email = Cypress.env('create-user_randomEmail');
    cy.get('#firstName').type('John');
    cy.get('#lastName').type('Doe');
    cy.get('#email').type(email);
    cy.get('#phonenumber').type('123456789');
    cy.get('#password').type('Password@123');
    cy.get('#confirmPassword').type('Password@123');
    cy.get('#role').select('Task Manager');

    cy.get('form').submit();

    cy.wait('@createUser').its('response.statusCode').should('eq', 401);

    cy.get('.text-error').should('contain', 'User already exists with email=', email);
  });
});
