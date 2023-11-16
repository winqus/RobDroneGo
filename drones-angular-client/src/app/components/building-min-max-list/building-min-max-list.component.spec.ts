import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BuildingMinMaxListComponent } from './building-min-max-list.component';

describe('BuildingMinMaxListComponent', () => {
  let component: BuildingMinMaxListComponent;
  let fixture: ComponentFixture<BuildingMinMaxListComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [BuildingMinMaxListComponent]
    });
    fixture = TestBed.createComponent(BuildingMinMaxListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
