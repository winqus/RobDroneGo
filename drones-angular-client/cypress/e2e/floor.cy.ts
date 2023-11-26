import { fakeCreds } from '../support/fake-data';

//Data used:
// Create a building with code A
// Create a floor with building code A and floor number 10
before(() => {
  cy.visit('/');
      cy.contains('Log In').click();
      cy.get('input[formcontrolname=email]').type(fakeCreds.email);
      cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
      
      cy.url().should('include', '/dashboard');
  
      cy.visit('/campus/building/create');

      cy.get('#buildingCode').type('A');
      cy.get('#buildingName').type('BuildingA');
      cy.get('#buildingDescription').type('Building A');
      cy.get('#floorSizeLength').type('10');
      cy.get('#floorSizeWidth').type('10');

      cy.contains('Create Building').click();
      cy.wait(2000);

});

describe('Navigation to Create Floor Page', () => {
    it('should navigate to Floor Create page', () => {
      // cy.login(fakeCreds.email, fakeCreds.password);
  
      cy.visit('/');
      // cy.contains('Log In').click();
      // cy.get('input[formcontrolname=email]').type(fakeCreds.email);
      // cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
      
      cy.url().should('include', '/dashboard');
  
      cy.visit('/campus/floor/create');
      
      cy.url().should('include', '/campus/floor/create');

      cy.contains('Create Floor').should('have.attr', 'disabled');
      
    });
});

describe('Floor Creation', () => {
  it('should submit the Floor Create form', () => {
    // cy.login(fakeCreds.email, fakeCreds.password);

    cy.visit('/');
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
    cy.contains('Floor created successfully').should('be.visible');
  });

  it('should block submit button if description is too long', () => {
    // cy.login(fakeCreds.email, fakeCreds.password);

    cy.visit('/');
    cy.contains('Log In').click();
    cy.get('input[formcontrolname=email]').type(fakeCreds.email);
    cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
    
    cy.url().should('include', '/dashboard');

    cy.visit('/campus/floor/create');
    
    cy.url().should('include', '/campus/floor/create');
    
    // Fill out the Robot Create form
    cy.get('#buildingCode').select('A');
    cy.get('#floorDescription').type('Floor 1'.repeat(100));
    cy.get('#floorNumber').type('10');
    // Add more fields as needed

    // Submit the form
    cy.contains('Create Floor').should('have.attr', 'disabled');
    
    // Assert that a success message is displayed
    cy.contains('Max 255 characters allowed.').should('be.visible');
  });

});

describe('List floors of the building', () => {

  it('should list floors of the building', () => {

    cy.visit('/');
    cy.contains('Log In').click();
    cy.get('input[formcontrolname=email]').type(fakeCreds.email);
    cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
    cy.url().should('include', '/dashboard');

    cy.visit('/campus/building/list');
    cy.url().should('include', '/campus/building/list');
    
    // cy.wait(2000);
    
    // cy.get('.class', { timeout: 12000 }).should('be.visible')
    cy.contains('Code: A', { timeout: 1000 }).click({force: true});
    cy.contains('See Floors', { timeout: 1000 }).click({force: true});
    cy.url().should('include', '/campus/building/A/floors');

    cy.contains('Floor Number: 10').should('be.visible');
    
    // cy.wait(500);
  });

  it('should show a message if building does not exist', () => {

    cy.visit('/');
    cy.contains('Log In').click();
    cy.get('input[formcontrolname=email]').type(fakeCreds.email);
    cy.get('input[formcontrolname=password]').type(fakeCreds.password).type('{enter}');
    cy.url().should('include', '/dashboard');

    cy.visit('/campus/building/Z/floors');
    
    cy.url().should('include', '/campus/building/Z/floors');

    cy.contains('exist or has no floors yet').should('be.visible');
    
    // cy.wait(500);
  });

});
  