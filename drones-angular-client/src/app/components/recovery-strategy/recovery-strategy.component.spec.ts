import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RecoveryStrategyComponent } from './recovery-strategy.component';

describe('RecoveryStrategyComponent', () => {
  let component: RecoveryStrategyComponent;
  let fixture: ComponentFixture<RecoveryStrategyComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [RecoveryStrategyComponent]
    });
    fixture = TestBed.createComponent(RecoveryStrategyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
