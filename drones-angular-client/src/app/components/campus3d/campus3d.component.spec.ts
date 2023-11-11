import { ComponentFixture, TestBed } from '@angular/core/testing';

import { Campus3dComponent } from './Campus3dComponent';

describe('Campus3dComponent', () => {
  let component: Campus3dComponent;
  let fixture: ComponentFixture<Campus3dComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [Campus3dComponent]
    });
    fixture = TestBed.createComponent(Campus3dComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
