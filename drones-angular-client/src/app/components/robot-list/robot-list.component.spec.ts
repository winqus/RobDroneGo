import { ComponentFixture, TestBed } from '@angular/core/testing';
import { of } from 'rxjs';
import Robot from '../../core/models/robot.model';
import { RobotService } from '../../services/robot.service';
import { RobotListComponent } from './robot-list.component';

describe('RobotListComponent', () => {
  let component: RobotListComponent;
  let fixture: ComponentFixture<RobotListComponent>;
  let mockRobotService: jasmine.SpyObj<RobotService>;
  const mockRobots: Robot[] = [
    { id: 'r1', code: 'Robot 1', description: 'Description 1', nickname: 'Robot 1', serialNumber: '123456789', available: true, type: 'type 1' },
    { id: 'r2', code: 'Robot 2', description: 'Description 2', nickname: 'Robot 2', serialNumber: '123456789', available: true, type: 'type 2' },
    { id: 'r3', code: 'Robot 3', description: 'Description 3', nickname: 'Robot 3', serialNumber: '123456789', available: true, type: 'type 3' },
  ];

  beforeEach(() => {
    mockRobotService = jasmine.createSpyObj(['getRobot']);
    TestBed.configureTestingModule({
      declarations: [RobotListComponent],
      providers: [{ provide: RobotService, useValue: mockRobotService }],
    });
    fixture = TestBed.createComponent(RobotListComponent);
    component = fixture.componentInstance;
    mockRobotService.getRobot.and.returnValue(of(mockRobots));
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load robots on init', () => {
    expect(component.robots.length).toBe(mockRobots.length);
    expect(component.robots).toEqual(mockRobots);
  });
});
