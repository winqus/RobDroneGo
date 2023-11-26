import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PathsBetweenBuildingsComponent } from './paths-between-buildings.component';

describe('PathsBetweenBuildingsComponent', () => {
  let component: PathsBetweenBuildingsComponent;
  let fixture: ComponentFixture<PathsBetweenBuildingsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [PathsBetweenBuildingsComponent]
    });
    fixture = TestBed.createComponent(PathsBetweenBuildingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
