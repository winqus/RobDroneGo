import { fakeCreds } from '../support/fake-data';

//Data used:
// Create a building with code Y


describe('Building List tests', () => {
    it('should navigate to List Buildings page', () => {
  
        cy.visit('/');
        cy.contains('Log In').click();
        cy.get('input[formcontrolname=email]').type(fakeCreds.email);
        cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
        cy.url().should('include', '/dashboard');
    
      cy.url().should('include', '/dashboard');
  
      cy.visit('/campus/building/list');
      
      cy.url().should('include', '/campus/building/list');
      
    });

    it('should show a message if building does not exist', () => {

        cy.visit('/');
        cy.contains('Log In').click();
        cy.get('input[formcontrolname=email]').type(fakeCreds.email);
        cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
        cy.url().should('include', '/dashboard');
    
        cy.visit('/campus/building/list');
        
        cy.url().should('include', '/campus/building/list');
    
        cy.contains('No buildings were created yet').should('be.visible');

      });

    it('should list buildings', () => {

        cy.visit('/');
        cy.contains('Log In').click();
        cy.get('input[formcontrolname=email]').type(fakeCreds.email);
        cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
        cy.url().should('include', '/dashboard');

        cy.visit('/campus/building/create');

        cy.get('#buildingCode').type('Y');
        cy.get('#buildingName').type('BuildingY');
        cy.get('#buildingDescription').type('Building Y');
        cy.get('#floorSizeLength').type('10');
        cy.get('#floorSizeWidth').type('10');
  
        cy.contains('Create Building').click();
        cy.wait(2000);
    
        cy.visit('/campus/building/list');
        cy.url().should('include', '/campus/building/list');
    
        cy.contains('Code: Y').should('be.visible');

      });
});
