import LoginCredentials from './loginCreds.interface';

export function registerCommands() {
  Cypress.Commands.add('login', (loginCreds: LoginCredentials) => {
    cy.session(
      [loginCreds],
      () => {
        const { email, password } = loginCreds;

        cy.log(`Logging in as ${email}`);

        cy.request({
          method: 'POST',
          url: '/api/auth/signin',
          body: { email, password },
        }).then(({ body }) => {
          window.localStorage.setItem('JWT_TOKEN', body.token);
        });

        cy.wait(100);

        // Save JWT Token after successful login
        cy.window().then((window) => {
          const token = window.localStorage.getItem('JWT_TOKEN');
          Cypress.env('JWT_TOKEN', token); // Store token in Cypress environment
        });
      },
      {
        validate() {
          cy.visit('/');
          cy.url().should('include', '/dashboard');
        },
      },
    );
  });

  Cypress.Commands.add('createBuilding', (buildingCode: string, floorSizeLength = 100, floorSizeWidth = 100) => {
    cy.visit('/campus/building/create');

    cy.get('#buildingCode').type(buildingCode);
    cy.get('#buildingName').type('Building' + buildingCode);
    cy.get('#buildingDescription').type('Description of Building ' + buildingCode);
    cy.get('#floorSizeLength').type(floorSizeLength.toString());
    cy.get('#floorSizeWidth').type(floorSizeWidth.toString());

    cy.intercept('POST', '/api/building').as('createBuilding');
    cy.contains('Create Building').click();
    cy.wait('@createBuilding');
    cy.visit('/');
  });

  Cypress.Commands.add('createFloor', (buildingCode: string, floorNumber: number) => {
    cy.visit('/campus/floor/create');

    cy.get('#buildingCode').select(buildingCode);
    cy.get('#floorNumber').type(floorNumber.toString());
    cy.get('#floorDescription').type('Floor ' + floorNumber);

    cy.intercept('POST', '/api/floor').as('createFloor');
    cy.contains('Create Floor').click();
    cy.wait('@createFloor');
    cy.visit('/');
  });
}
