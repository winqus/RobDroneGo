import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AppBuildingFloorDropdownListComponent } from './app-building-floor-dropdown-list.component';

describe('AppBuildingFloorDropdownListComponent', () => {
  let component: AppBuildingFloorDropdownListComponent;
  let fixture: ComponentFixture<AppBuildingFloorDropdownListComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [AppBuildingFloorDropdownListComponent]
    });
    fixture = TestBed.createComponent(AppBuildingFloorDropdownListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
