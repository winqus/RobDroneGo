import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SearchRobotComponent } from './search-robot.component';

describe('SearchRobotComponent', () => {
  let component: SearchRobotComponent;
  let fixture: ComponentFixture<SearchRobotComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [SearchRobotComponent]
    });
    fixture = TestBed.createComponent(SearchRobotComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
