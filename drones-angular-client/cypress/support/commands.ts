// ***********************************************
// This example namespace declaration will help
// with Intellisense and code completion in your
// IDE or Text Editor.
// ***********************************************

// declare namespace Cypress {
//   interface Chainable<Subject = any> {
//     customCommand(param: any): typeof customCommand;
//   }
// }

// function customCommand(param: any): void {
//   console.warn(param);
// }


//
// NOTE: You can use it like so:
// Cypress.Commands.add('customCommand', customCommand);
//
// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
Cypress.Commands.add('login' as any, (username, password) => {
    cy.visit('/login')
  
    cy.get('input[name=username]').type(username as any)
  
    // {enter} causes the form to submit
    cy.get('input[name=password]').type(`${password}{enter}`, { log: false })
  
    // we should be redirected to /dashboard
    cy.url().should('include', '/dashboard')
  
    // our auth cookie should be present
    cy.getCookie('your-session-cookie').should('exist')
  
    // UI should reflect this user being logged in
    cy.get('h1').should('contain', username)
  })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })
