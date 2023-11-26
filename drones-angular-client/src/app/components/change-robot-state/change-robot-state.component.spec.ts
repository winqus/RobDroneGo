import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ChangeRobotStateComponent } from './change-robot-state.component';

describe('ChangeRobotStateComponent', () => {
  let component: ChangeRobotStateComponent;
  let fixture: ComponentFixture<ChangeRobotStateComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ChangeRobotStateComponent]
    });
    fixture = TestBed.createComponent(ChangeRobotStateComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
