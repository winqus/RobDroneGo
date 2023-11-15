import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FloorsToDifBuildsComponent } from './floors-to-dif-builds.component';

describe('FloorsToDifBuildsComponent', () => {
  let component: FloorsToDifBuildsComponent;
  let fixture: ComponentFixture<FloorsToDifBuildsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [FloorsToDifBuildsComponent]
    });
    fixture = TestBed.createComponent(FloorsToDifBuildsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
