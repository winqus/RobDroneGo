import { ComponentFixture, TestBed } from '@angular/core/testing';

import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { of } from 'rxjs';
import Building from '../../core/models/building.model';
import BuildingService from '../../services/building.service';
import { BuildingListComponent } from './building-list.component';

describe('BuildingListComponent', () => {
  let component: BuildingListComponent;
  let fixture: ComponentFixture<BuildingListComponent>;
  let mockBuildingService: any;
  let mockRouter: any;
  const mockBuildings: Building[] = [
    { id: 'b-1', code: 'B1', name: 'Building 1', description: 'Desc 1', floorSizeLength: 100, floorSizeWidth: 200 },
    { id: 'b-2', code: 'B2', name: 'Building 2', description: 'Desc 2', floorSizeLength: 150, floorSizeWidth: 250 },
  ];

  beforeEach(() => {
    mockBuildingService = jasmine.createSpyObj(['getAllBuildings']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [BuildingListComponent],
      providers: [
        { provide: BuildingService, useValue: mockBuildingService },
        { provide: Router, useValue: mockRouter },
      ],
      imports: [RouterTestingModule],
    });

    fixture = TestBed.createComponent(BuildingListComponent);
    component = fixture.componentInstance;
    mockBuildingService.getAllBuildings.and.returnValue(of(mockBuildings));
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load buildings on init', () => {
    expect(component.buildings.length).toBe(2);
    expect(component.buildings).toEqual(mockBuildings);
  });

  it('should navigate to edit building', () => {
    const building = mockBuildings[0];
    component.editBuilding(building);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', building.code, 'edit'], { state: { data: building } });
  });

  it('should navigate to create floor', () => {
    const buildingCode = 'B1';
    component.createFloor(buildingCode);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/floor/create']);
  });

  it('should navigate to see floors of a building', () => {
    const buildingCode = 'B1';
    component.getByBuildingCode(buildingCode);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', buildingCode, 'floors']);
  });

  it('should navigate to list elevators in a building', () => {
    const buildingCode = 'B1';
    component.listElevatorsInBuilding(buildingCode);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', buildingCode, 'elevators']);
  });

  it('should navigate to list floors with passages to other buildings', () => {
    const buildingCode = 'B1';
    component.listFloorsWithPassagesToOtherBuildings(buildingCode);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', buildingCode, 'floorsWithPassages']);
  });

  it('should display no buildings message when there are no buildings', () => {
    // Simulate no buildings
    component.buildings = [];
    fixture.detectChanges();

    // Query for the specific element, in the 'noBuildings' template, in this case <p> tag with class text-4xl
    const noBuildingsElement = fixture.debugElement.query(By.css('p.text-4xl'));
    expect(noBuildingsElement).toBeTruthy();
    expect(noBuildingsElement.nativeElement.textContent).toContain('No buildings were created yet.');
  });
});
