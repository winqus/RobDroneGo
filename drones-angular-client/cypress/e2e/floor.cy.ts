import { fakeCreds } from '../support/fake-data';

describe('Navigation to Create Floor Page', () => {
    it('should navigate to Floor Create page', () => {
  
      cy.visit('/');
      // press button with text Log In
      cy.contains('Log In').click();
  
      cy.get('input[formcontrolname=email]').type(fakeCreds.email);
      cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
      
      cy.url().should('include', '/dashboard');
  
      cy.visit('/campus/floor/create');
      
      cy.url().should('include', '/campus/floor/create');

      cy.contains('Create Floor').should('have.attr', 'disabled');
      
    });
});

  describe('Floor Create Form Submission', () => {
    it('should submit the Robot Create form', () => {

        cy.visit('/');
      // press button with text Log In
      cy.contains('Log In').click();
  
      cy.get('input[formcontrolname=email]').type(fakeCreds.email);
      cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
      
      cy.url().should('include', '/dashboard');

       cy.visit('/campus/floor/create');
      
      cy.url().should('include', '/campus/floor/create');
      
      // Fill out the Robot Create form
      cy.get('#buildingCode').select('A');
      cy.get('#floorNumber').type('10');
      cy.get('#floorDescription').type('Floor 1');
      // Add more fields as needed

      // Submit the form
      cy.contains('Create Floor').click();
      
      // Assert that a success message is displayed
      cy.contains('Floor created successfully');
    });
});
  