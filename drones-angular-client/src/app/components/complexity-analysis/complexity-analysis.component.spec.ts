import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ComplexityAnalysisComponent } from './complexity-analysis.component';

describe('ComplexityAnalysisComponent', () => {
  let component: ComplexityAnalysisComponent;
  let fixture: ComponentFixture<ComplexityAnalysisComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ComplexityAnalysisComponent]
    });
    fixture = TestBed.createComponent(ComplexityAnalysisComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
