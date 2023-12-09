import { TestBed } from '@angular/core/testing';

import { TaskRequestService } from './task-request.service';

describe('TaskRequestService', () => {
  let service: TaskRequestService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(TaskRequestService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
